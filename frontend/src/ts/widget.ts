import {Area, OutputArea, LabContainer} from './areas';
import {ButtonGroup, CheckBox, Tabs} from './components';
import {Editor, EditorTheme} from './editor';
import {fetchBlob, DownloadRequest, DownloadResponse} from './comms';
import {Resource, ResourceSet} from './resource';
import {ServerWorker} from './server';
import {RunProgram, CheckOutput} from './server-types';
import * as Strings from './strings';

enum DownloadType {
  None,
  Client,
  Server,
}

/**
 * Defines the widget behavior
 *
 * @export
 * @class Widget
 */
export class Widget {
  private editors: Array<Editor> = [];
  protected readonly container: HTMLDivElement;
  private readonly name: string | null;
  private readonly switches: Array<string> = [];
  private tabs = new Tabs();
  protected outputArea = new OutputArea();

  protected buttons = new ButtonGroup();

  private readonly dlType: DownloadType = DownloadType.Client;

  private readonly server: string;

  private shadowFiles: ResourceSet;
  protected outputGroup: HTMLDivElement | null = null;

  private depWidgets: WidgetList = [];

  /**
   * Creates an instance of Widget.
   *
   * @param {HTMLDivElement} elem - the container for the widget
   * @param {string} url - the server address:port
   */
  constructor(elem: HTMLDivElement, url: string) {
    const resources = new ResourceSet();
    this.server = url;
    this.container = elem;

    // Read attributes from container object to initialize members
    this.name = this.container.getAttribute('name');

    const files = this.container.getElementsByClassName('file');
    for (let i = 0; i < files.length; i++) {
      const file = files[i];
      if (!file.hasAttribute('basename') || file.textContent == null) {
        throw Error('Malform widget: File doesnt have a name');
      }
      const a: Resource = {
        basename: file.getAttribute('basename') as string,
        contents: file.textContent,
      };
      resources.addUnique(a);
    }

    if (resources.length == 0) {
      throw Error('Malformed widget: No files present.');
    }

    // Remove the files from the container since they are now editors
    while (files.length > 0) {
      this.container.removeChild(files[0]);
    }

    this.shadowFiles = new ResourceSet();
    const shadowFiles = this.container.getElementsByClassName('shadow_file');
    for (let i = 0; i < shadowFiles.length; i++) {
      const file = shadowFiles[i];
      if (!file.hasAttribute('basename') || file.textContent == null) {
        throw Error('Malform widget: File doesnt have a name');
      }
      const a: Resource = {
        basename: file.getAttribute('basename') as string,
        contents: file.textContent,
      };
      this.shadowFiles.addUnique(a);
    }

    // Remove the shadow files from the container since they are now a list
    while (shadowFiles.length > 0) {
      this.container.removeChild(shadowFiles[0]);
    }

    // fill the contents of the tabs
    for (const file of resources) {
      const ed = new Editor(file);
      this.editors.push(ed);

      const tab = this.tabs.addTab(file.basename, ed.render(), () => {
        const lengths = this.editors.map((e) => e.getLength());
        const max = Math.max(...lengths);
        ed.setLength(max);
      });
      ed.setTab(tab);
    }

    // Check which buttons are enabled on container and populate
    for (const mode in Strings.modeDictionary) {
      if (this.container.getAttribute(mode + '_button')) {
        this.buttons.addButton([],
            Strings.modeDictionary[mode].tooltip,
            Strings.modeDictionary[mode].buttonText,
            'click', async () => {
              await this.buttonCB(mode);
            });
      }
    }

    // if this widget doesn't have a name defined, don't allow for download
    if (this.name == null) {
      this.dlType = DownloadType.None;
    } else {
      // if there are any buttons, the dltype needs to be server
      if (this.buttons.length() > 0) {
        this.dlType = DownloadType.Server;
      } else {
        this.dlType = DownloadType.Client;
      }
    }

    // check for defined switches in attriburtes
    const swStr = this.container.getAttribute('switches');
    if (swStr) {
      this.switches = swStr.split(';');
    }
  }

  /**
   *  Method to destruct the object. Used primarily for testing.
   */
  public destructor(): void {
    for (const ed of this.editors) {
      ed.destructor();
    }
  }

  /**
   * Adds a list of dependecy widgets to this widget
   *
   * @param {WidgetList} list - the list of widgets that this widget depends on
   */
  public addDepWidgets(list: WidgetList): void {
    this.depWidgets.concat(list);
  }

  /**
   * Gets the visible files from the lsit of dependency widgets
   *
   * @private
   * @return {ResourceSet} - The set of files from dependent widgets
   */
  private getDepResources(): ResourceSet {
    const set = new ResourceSet();
    for (const w of this.depWidgets) {
      set.addListOverwrite(w.getVisibleResources());
    }

    return set;
  }

  /**
   * Gets the visible resources in the current Widget and returns the list
   *
   * @return {ResourceSet} - The list of files
   */
  public getVisibleResources(): ResourceSet {
    const files = new ResourceSet;
    this.editors.map((e) => {
      files.addUnique(e.getResource());
    });

    return files;
  }

  /**
   * Collect the resources loaded in the widget and add to the resources
   *  from dependent widgets and shadow files
   *
   * @return {ResourceSet} return the widget resources
   */
  protected collectResources(): ResourceSet {
    const visibleFiles = this.getVisibleResources();
    const depFiles = this.getDepResources();
    depFiles.addListOverwrite(visibleFiles);
    depFiles.addListOverwrite(this.shadowFiles);
    return depFiles;
  }

  /**
   * Construct the server address string
   *
   * @param {string} url - the url suffix
   * @return {string} - the full constructed url
   */
  private serverAddress(url: string): string {
    return this.server + '/' + url + '/';
  }

  /**
   * The main callback for the widget buttons
   *
   * @param {string} mode - the mode of the button that triggered the event
   * @param {boolean} lab - specifies if this is a lab widget
   */
  protected async buttonCB(mode: string, lab = false): Promise<void> {
    this.outputArea.reset();

    // Clear any annotations added from previous button click
    this.editors.map((e) => {
      e.clearGutterAnnotation();
    });

    this.outputArea.add(['output_info', 'console_output'],
        Strings.CONSOLE_OUTPUT_LABEL + ':');
    this.outputArea.showSpinner(true);

    const files = this.collectResources();

    const serverData: RunProgram.TS = {
      files: files.raw,
      mode: mode,
      switches: this.switches,
      name: this.name,
      lab: lab,
    };

    const worker = new ServerWorker(this.server,
        (data: CheckOutput.FS): number => {
          return this.processCheckOutput(data);
        });

    try {
      await worker.request(serverData, 'run_program');
    } catch (error) {
      this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
      console.error('Error:', error);
    } finally {
      this.outputArea.showSpinner(false);
    }
  }

  /**
   * The download example callback
   *
   * @private
   * @return {Promise<Array<DownloadResponse>>} - A promise of the dl response
   */
  private async downloadExample(): Promise<Array<DownloadResponse>> {
    const files = this.collectResources();
    const blobList: Array<DownloadResponse> = [];

    switch (this.dlType) {
      case DownloadType.None: {
        throw new Error('No download available for this exercise.');
      }
      case DownloadType.Server: {
        const serverData: DownloadRequest = {
          files: files.raw,
          switches: this.switches,
          name: this.name,
        };

        const ret = await fetchBlob(serverData,
            this.serverAddress('download'));
        blobList.push(ret);

        break;
      }
      case DownloadType.Client: {
        this.editors.map((e): void => {
          const resource: Resource = e.getResource();

          blobList.push({
            blob: new Blob([resource.contents], {type: 'text/plain'}),
            filename: resource.basename,
          });
        });
        break;
      }
    }

    return blobList;
  }

  /**
   * Returns the correct Area to place data in
   *
   * @param {number} ref - should be null for Widget
   * @return {Area} the area to place returned data
   */
  protected getHomeArea(ref: number): Area {
    if (ref != null) {
      throw new Error('Malformed data packet has ref in non-lab.');
    }
    return this.outputArea;
  }
  /**
   * Maps a basename to the corresponding editor
   *
   * @private
   * @param {string} basename - The basename to search for
   * @return {(Editor | null)} - Return the editor of null if not found
   */
  private basenameToEditor(basename: string): Editor | null {
    for (const e of this.editors) {
      if (basename === e.getResource().basename) {
        return e;
      }
    }

    return null;
  }

  /**
   * Handle the msg data coming back from server
   *
   * @param {CheckOutput.RunMsg} msg - the returned msg
   * @param {Area} homeArea - the area to place the rendered msg
   */
  protected handleMsgType(msg: CheckOutput.RunMsg, homeArea: Area): void {
    switch (msg.type) {
      case 'console': {
        homeArea.addConsole(msg.data);
        break;
      }
      case 'internal_error':
        msg.data += ' ' + Strings.INTERNAL_ERROR_MESSAGE;
        // Intentional: fall through
      case 'stderr':
      case 'stdout': {
        // Split multiline messages into single lines for processing
        const outMsgList = msg.data.split(/\r?\n/);
        for (const outMsg of outMsgList) {
          const ctRegex = /^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/m;
          const rtRegex = /^raised .+ : ([a-zA-Z._0-9-]+):(\d+) (.+)$/m;
          const ctMatchFound = outMsg.match(ctRegex);
          const rtMatchFound = outMsg.match(rtRegex);
          // Lines that contain a sloc are clickable:
          const cb = (row: number, col: number, ed: Editor | null): void => {
            if (window.getSelection()?.toString() == '') {
              ed?.getTab()?.click();
              // Jump to corresponding line
              ed?.gotoLine(row, col);
            }
          };

          if (ctMatchFound?.length == 5) {
            const basename = ctMatchFound[1];
            const editor = this.basenameToEditor(basename);
            const row = parseInt(ctMatchFound[2]);
            const col = parseInt(ctMatchFound[3]);

            if (ctMatchFound[4].indexOf(' info:') === 0) {
              homeArea.addInfo(outMsg, () => {
                cb(row, col, editor);
              });
              editor?.setGutterAnnotation(row, col, outMsg, 'info');
            } else {
              if (ctMatchFound[4].indexOf(' warning:') === 0) {
                editor?.setGutterAnnotation(row, col, outMsg, 'warning');
              } else {
                editor?.setGutterAnnotation(row, col, outMsg, 'error');
              }
              homeArea.addMsg(outMsg, () => {
                cb(row, col, editor);
              });
            }
          } else if (rtMatchFound?.length == 4) {
            const basename = rtMatchFound[1];
            const editor = this.basenameToEditor(basename);
            const row = parseInt(rtMatchFound[2]);
            const col = 1;

            homeArea.addMsg(outMsg, () => {
              cb(row, col, editor);
            });
            editor?.setGutterAnnotation(row, col, outMsg, 'error');
          } else {
            homeArea.addLine(outMsg);
          }
        }
        break;
      }
      default: {
        homeArea.addLine(msg.data);
        throw new Error('Unhandled msg type.');
      }
    }
  }

  /**
   * Process the output from "check_output" ajax request
   * @param {CheckOutput.FS} data - The data from check_output
   * @return {number} the number of lines read by this function
   */
  private processCheckOutput(data: CheckOutput.FS): number {
    let readLines = 0;

    data.output.map((ol) => {
      const homeArea = this.getHomeArea(ol.ref);
      readLines++;

      this.handleMsgType(ol.msg, homeArea);
    });

    if (data.completed) {
      if (data.status != 0) {
        this.outputArea.addError(Strings.EXIT_STATUS_LABEL +
            ': ' + data.status);
      }
    }

    return readLines;
  }

  /**
   * Reset the editors, outputArea
   */
  protected resetEditors(): void {
    this.outputArea.reset();

    this.editors.map((e) => {
      e.reset();
    });
  }

  /**
   * Render the settings bar for the widget
   * @return {HTMLDivElement} the rendered settings bar
   */
  private renderSettingsBar(): HTMLDivElement {
    const settingsBar = document.createElement('div');
    settingsBar.classList.add('settings-bar');

    const dropdownContainer = document.createElement('div');
    dropdownContainer.classList.add('dropdown-container', 'settingsbar-item');
    settingsBar.appendChild(dropdownContainer);

    const dropdownButton = document.createElement('button');
    dropdownButton.classList.add('dropdown-btn');
    dropdownButton.innerHTML = '<i class="fas fa-cog"></i>';
    dropdownContainer.appendChild(dropdownButton);

    const dropdownContent = document.createElement('div');
    dropdownContent.classList.add('dropdown-content');
    dropdownContainer.appendChild(dropdownContent);

    const tabSetting =
        new CheckBox(Strings.SETTINGS_TABBED_EDITOR_LABEL, dropdownContent);
    tabSetting.getCheckBox().checked = true;
    tabSetting.getCheckBox().addEventListener('change', () => {
      if (tabSetting.checked()) {
        this.tabs.show(true);
      } else {
        this.tabs.show(false);
      }
    });

    const themeSetting =
        new CheckBox(Strings.SETTINGS_THEME_EDITOR_LABEL, dropdownContent);

    themeSetting.getCheckBox().addEventListener('change', () => {
      let theme = EditorTheme.Light;
      if (themeSetting.checked()) {
        theme = EditorTheme.Dark;
      }
      this.editors.map((e) => {
        e.setTheme(theme);
      });
    });

    const resetButton = document.createElement('button');
    resetButton.setAttribute('type', 'button');
    resetButton.classList.add('settingsbar-item', 'reset-btn');
    resetButton.setAttribute('title', Strings.RESET_TOOLTIP);
    resetButton.innerHTML = '<i class="fas fa-undo"></i>';
    settingsBar.appendChild(resetButton);
    resetButton.addEventListener('click', () => {
      if (window.confirm(Strings.RESET_CONFIRM_MSG)) {
        this.resetEditors();
      }
    });

    if (this.dlType != DownloadType.None) {
      const dlButton = document.createElement('button');
      dlButton.setAttribute('type', 'button');
      dlButton.classList.add('settingsbar-item', 'download-btn');
      dlButton.setAttribute('title', Strings.DOWNLOAD_TOOLTIP);
      dlButton.innerHTML = '<i class="fas fa-file-download"></i>';
      settingsBar.appendChild(dlButton);
      dlButton.addEventListener('click', async () => {
        try {
          const blobs = await this.downloadExample();

          for (const blob of blobs) {
            const objURL: string = URL.createObjectURL(blob.blob);

            const a = document.createElement('a');
            a.setAttribute('href', objURL);
            a.setAttribute('download', blob.filename);
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);

            URL.revokeObjectURL(objURL);
          }
        } catch (error) {
          this.outputArea.reset();
          this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
          console.error('Error:', error);
        }
      });
    }

    return settingsBar;
  }

  /**
   * Render the widget by putting it into this.container
   */
  public render(): void {
    this.tabs.render(this.container);
    this.container.appendChild(this.renderSettingsBar());

    const row = document.createElement('div');
    row.classList.add('row', 'output_row');
    this.container.appendChild(row);

    row.appendChild(this.buttons.render());

    this.outputGroup = document.createElement('div');
    this.outputGroup.classList.add('col-md-9');
    this.outputGroup.appendChild(this.outputArea.render());
    row.appendChild(this.outputGroup);
  }
}

/**
 * The LabWidget class
 * @extends Widget
 */
export class LabWidget extends Widget {
  private readonly labContainer: LabContainer = new LabContainer;

  /**
   * Constructs the LabWidget
   * @param {HTMLDivElement} container - the container for the widget
   * @param {string} server - the server address:port
   */
  constructor(container: HTMLDivElement, server: string) {
    super(container, server);

    this.buttons.addButton([],
        Strings.modeDictionary['submit'].tooltip,
        Strings.modeDictionary['submit'].buttonText,
        'click', async () => {
          await this.buttonCB('submit');
        });
  }

  /**
   * The main callback for the widget buttons
   * @param {string} mode - the mode of the button that triggered the event
   * @param {boolean} lab - specifies that this is a lab
   */
  protected async buttonCB(mode: string, lab = true): Promise<void> {
    this.labContainer.reset();

    await super.buttonCB(mode, lab);

    this.labContainer.sort();
  }

  /**
   * Returns the correct Area to place data in
   * @param {number} ref - if not null, the lab ref
   * @return {Area} the area to place returned data
   */
  protected getHomeArea(ref: number): Area {
    if (ref != null) {
      return this.labContainer.getLabArea(ref);
    }
    return this.outputArea;
  }

  /**
   * Handle the msg data coming back from server
   * @param {CheckOutput.RunMsg} msg - the returned msg
   * @param {Area} homeArea - the area to place the rendered msg
   */
  protected handleMsgType(msg: CheckOutput.RunMsg, homeArea: Area): void {
    switch (msg.type) {
      case 'lab': {
        const result =
          this.labContainer.processResults(
              (msg.data as unknown) as CheckOutput.LabOutput);
        this.outputArea.addLabStatus(result);
        break;
      }
      default: {
        super.handleMsgType(msg, homeArea);
      }
    }
  }

  /**
   * Reset the editors, outputArea, and labContainer
   */
  protected resetEditors(): void {
    super.resetEditors();
    this.labContainer.reset();
  }

  /**
   * Render the widget by putting it into this.container
   */
  public render(): void {
    super.render();
    const lc = this.labContainer.render();
    this.outputGroup?.appendChild(lc);
  }
}

type WidgetLike = Widget | LabWidget;
type WidgetList = Array<WidgetLike>;

type WidgetMapElement = {
  name: string;
  widgets: WidgetList;
}

/**
 * Defines a map of widget name to list of associated widgets
 *
 * @export
 * @class WidgetMap
 * @implements {Iterable<WidgetMapElement>}
 */
export class WidgetMap implements Iterable<WidgetMapElement> {
  private counter = 0;
  private map: Array<WidgetMapElement> = [];

  /**
   * Returns the element matching the name parameter. If there is no matching
   *  element, the subprogram returns null.
   *
   * @param {string} name - the name of the widget to search for
   * @return {WidgetMapElement | null} - returns the element matched or null
   */
  private exists(name: string): WidgetMapElement | null {
    for (const elem of this.map) {
      if (elem.name === name) {
        return elem;
      }
    }

    return null;
  }

  /**
   * Adds a widget to the map. If the name of the widget is already in the map,
   *  the widget is added to the list of widgets with the name.
   *
   * @param {string} name - the name of the widget
   * @param {WidgetLike} widget - the widget to add to the map
   */
  public addWidget(name: string, widget: WidgetLike): void {
    const wme = this.exists(name);
    if (wme) {
      const wList = wme.widgets;
      // we already have another widget with the same name
      widget.addDepWidgets(wList);
      // then add the widget to the list
      wList.push(widget);
    } else {
      // this is the first widget with this name to be added
      this.map.push({
        name: name,
        widgets: [widget],
      });
    }
  }

  /**
   * Allows the WidgetMap to be Iterable
   *
   * @return {IteratorResult<WidgetMapElement>}
   */
  public next(): IteratorResult<WidgetMapElement> {
    if (this.counter < this.map.length) {
      return {
        done: false,
        value: this.map[this.counter++],
      };
    } else {
      this.counter = 0;
      return {
        done: true,
        value: null,
      };
    }
  }

  /**
   * Allows the ResourceSet to be looped over
   *
   * @return {IterableIterator<WidgetMapElement>}
   */
  [Symbol.iterator](): IterableIterator<WidgetMapElement> {
    return this;
  }

  /**
   * Returns the number of names in the map
   *
   * @readonly
   * @return {number} - The length of the set
   */
  get length(): number {
    return this.map.length;
  }

  /**
   * Gets the list of widgets associated with the name. Returns null if not
   *  found.
   *
   * @param {string} name - the name to find
   * @returns {(WidgetList | null)} - Returns the found list or null
   */
  public get(name: string): WidgetList | null {
    const res = this.exists(name);
    if(res) {
      return res.widgets;
    }

    return null;
  }
}

/**
 * Entrypoint for widget creation
 *
 * @export
 * @param {HTMLCollectionOf<Element>} widgets - The collection of widgets
 *    found on the page. This is the return value of getElementsByClass
 * @return {Array<Widget | LabWidget>} The list of widgets on the page
 */
export function widgetFactory(widgets: HTMLCollectionOf<Element>): WidgetMap {
  const widgetList = new WidgetMap();
  for (let i = 0; i < widgets.length; i++) {
    const element = (widgets[i] as HTMLDivElement);
    const server = element.getAttribute('example_server');
    const name = element.getAttribute('name');
    try {
      if (!server) {
        throw Error('Malformed widget! No server address specified.');
      }
      if (!name) {
        throw Error('Malformed widget! No widget name specified.');
      }
      const lab = element.getAttribute('lab');
      const widget =
          lab ? new LabWidget(element, server) : new Widget(element, server);

      widget.render();
      widgetList.addWidget(name, widget);
    } catch (error) {
      // an error has occured parsing the widget
      console.error('Error:', error);

      // clear the offending element to remove any processing that was done
      element.innerHTML = '';

      // add an error message to the page in its place
      const errorDiv = document.createElement('div');
      errorDiv.innerHTML = '<p>An error has occured processing this widget.' +
      Strings.INTERNAL_ERROR_MESSAGE + '</p>';

      element.appendChild(errorDiv);
    }
  }

  return widgetList;
}
