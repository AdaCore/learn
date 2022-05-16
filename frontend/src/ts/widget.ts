import {Area, OutputArea, LabContainer} from './areas';
import {Editor, EditorTheme} from './editor';
import {fetchBlob, DownloadRequest, DownloadResponse} from './comms';
import {getElemsByClass, getElemById, getElemsByTag}
  from './dom-utils';
import {Resource, ResourceList} from './resource';
import {ServerWorker} from './server';
import {RunProgram, CheckOutput} from './server-types';
import * as Strings from './strings';

interface EditorView {
  readonly header: HTMLButtonElement;
  readonly editor: Editor;
}

type EditorMap = Map<string, EditorView>;

/**
 * Defines the widget behavior
 *
 * @export
 * @class Widget
 */
class Widget {
  // model object
  private readonly name: string;
  private readonly main: string;
  private readonly server: string;
  private readonly id: string;
  private readonly shadowFiles: ResourceList = [];

  // view objects
  protected readonly container: HTMLDivElement;
  protected readonly outputArea: OutputArea;
  public readonly viewMap: EditorMap = new Map();

  // other object
  private readonly editor: Editor;

  /**
   * Creates an instance of Widget and attaches logic to DOM
   *
   * @param {HTMLDivElement} elem - the container for the widget
   * @param {(EditorMap | undefined)} dep - The view of widgets with the same
   *  name on the current page
   */
  constructor(elem: HTMLDivElement, dep: EditorMap | undefined) {
    this.server = elem.dataset.url as string;
    this.container = elem;

    // Read attributes from container object to initialize members
    this.id = this.container.id;
    this.name = elem.dataset.name as string;
    this.main = elem.dataset.main as string;

    // add widget dependencies from up page to the EditorView
    if (dep) {
      // Shallow copy the dep map into our map
      for (const [k, v] of dep.entries()) {
        this.viewMap.set(k, v);
      }
    }

    // Initialize editor
    const edDiv = this.getElem('editor') as HTMLDivElement;
    this.editor = new Editor(edDiv);

    // Parse files
    const files = getElemsByClass(this.container, 'file');
    // Check to make sure we have files in the widget
    if (files.length == 0) {
      throw Error('Malformed widget: No files present.');
    }
    for (const file of files) {
      const basename = file.dataset.basename as string;
      const content = file.textContent ? file.textContent : '';
      this.editor.addSession(basename, content);
    }

    // Parse shadow files
    const shadowFiles = getElemsByClass(this.container, 'shadow-file');
    for (const file of shadowFiles) {
      const a: Resource = {
        basename: file.dataset.basename as string,
        contents: file.textContent ? file.textContent : '',
      };
      this.shadowFiles.push(a);
    }

    // Setup editor tabs
    const tab = this.getElem('tab');
    const headers = getElemsByTag(tab, 'button');
    for (const h of headers) {
      const basename = h.textContent ? h.textContent : '';
      const newView: EditorView = {
        header: this.getElem('tab', basename) as HTMLButtonElement,
        editor: this.editor,
      };

      h.addEventListener('click', () => {
        for (const i of headers) {
          i.classList.remove('active');
        }
        h.classList.add('active');
        this.editor.setSession(basename);
      });

      this.viewMap.set(basename, newView);
    }

    // simulate click on the first tab to show it
    headers[0].click();

    // attach button logic
    const buttonGroup = this.getElem('button-group');
    const buttons = getElemsByTag(buttonGroup, 'button');
    for (const btn of buttons) {
      const mode = btn.dataset.mode as string;
      btn.addEventListener('click', async () => {
        await this.buttonCB(mode);
      });
    }

    // attach handlers to the settings bar items
    const tabSetting =
      this.getElem('settings-bar', 'tab-setting') as HTMLInputElement;
    tabSetting.checked = true;
    tabSetting.addEventListener('change', () => {
      // TODO: figure out how to do this
      // if (tabSetting.checked) {
      //   for (const t of this.viewMap.values()) {
      //     t.header.style.display = 'block';
      //     if (t.content.classList.contains('active')) {
      //       t.content.style.display = 'block';
      //     } else {
      //       t.content.style.display = 'none';
      //     }
      //   }
      // } else {
      //   for (const t of this.viewMap.values()) {
      //     t.header.style.display = 'none';
      //     t.content.style.display = 'block';
      //   }
      // }
    });

    const themeSetting =
      this.getElem('settings-bar', 'theme-setting') as HTMLInputElement;
    themeSetting.addEventListener('change', () => {
      let theme = EditorTheme.Light;
      if (themeSetting.checked) {
        theme = EditorTheme.Dark;
      }
      for (const t of this.viewMap.values()) {
        t.editor.setTheme(theme);
      }
    });

    const resetButton =
      this.getElem('settings-bar', 'reset-btn') as HTMLButtonElement;
    resetButton.addEventListener('click', () => {
      if (window.confirm(Strings.RESET_CONFIRM_MSG)) {
        this.resetEditors();
      }
    });

    const dlButton = this.getElem('settings-bar', 'download-btn');
    dlButton.addEventListener('click', async () => {
      try {
        const blob = await this.downloadExample();
        const objURL: string = URL.createObjectURL(blob.blob);

        const a = document.createElement('a');
        a.setAttribute('href', objURL);
        a.setAttribute('download', blob.filename);
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);

        URL.revokeObjectURL(objURL);
      } catch (error) {
        this.outputArea.reset();
        this.outputArea.addError(Strings.MACHINE_BUSY_LABEL);
        console.error('Error:', error);
      }
    });

    // grab reference to output area in the HTML and construct area
    const outputArea = this.getElem('output-area') as HTMLDivElement;
    this.outputArea = new OutputArea(outputArea);
  }

  /**
   * Collect resources from the current view
   *
   * @return {ResourceList} return the widget resources
   */
  protected collectResources(): ResourceList {
    const ret: ResourceList = [];
    // get files from view
    for (const [basename, view] of this.viewMap) {
      const r: Resource = {
        basename: basename,
        contents: view.editor.getSessionContent(basename),
      };
      ret.push(r);
    }
    // add shadow files
    for (const sf of this.shadowFiles) {
      ret.push(sf);
    }
    // TODO: add cli contents to files
    return ret;
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
   * Gets an element by its id inside the current widget layout
   *
   * The ids inside the widget are in the form:
   * <widget number>.<item>.<sub item>
   *
   * This function will prepend the widget number to the args passed in.
   * An example would be:
   *
   * this.getElem('foo', 'bar) would return an element with the ID
   * <widget number>.foo.bar
   *
   * @protected
   * @param {...Array<string>} args - The list of args to append
   * @return {HTMLElement} - The element with the ID
   */
  protected getElem(...args: Array<string>): HTMLElement {
    const fullId = [this.id].concat(args).join('.');
    return getElemById(fullId);
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
    for (const t of this.viewMap.values()) {
      t.editor.clearGutterAnnotation();
    }

    this.outputArea.add(['output_info', 'console_output'],
        Strings.CONSOLE_OUTPUT_LABEL + ':');
    this.outputArea.showSpinner(true);

    const files = this.collectResources();

    const serverData: RunProgram.TSData = {
      files: files,
      main: this.main,
      mode: mode,
      switches: JSON.parse(this.container.dataset.switches as string),
      name: this.name,
      lab: lab,
    };

    const worker = new ServerWorker(this.server,
        (data: CheckOutput.FS): boolean => {
          return this.processCheckOutput(data);
        });

    try {
      await worker.execute(serverData);
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
   * @return {Promise<DownloadResponse>} - A promise of the dl response
   */
  private async downloadExample(): Promise<DownloadResponse> {
    const files = this.collectResources();

    const serverData: DownloadRequest = {
      files: files,
      switches: JSON.parse(this.container.dataset.switches as string),
      name: this.name,
    };

    return fetchBlob(serverData, this.serverAddress('download'));
  }

  /**
   * Returns the correct Area to place data in
   *
   * @param {CheckOutput.FS} data - should be null for Widget
   * @return {Area} the area to place returned data
   */
  protected getHomeArea(data: CheckOutput.FS): Area {
    if (data.ref !== undefined) {
      throw new Error('Malformed data packet has ref in non-lab.');
    }
    return this.outputArea;
  }

  /**
   * Handle the msg data coming back from server
   *
   * @param {CheckOutput.RunMsg} msg - the returned msg
   * @param {Area} homeArea - the area to place the rendered msg
   */
  protected handleMsgType(msg: CheckOutput.RunMsg, homeArea: Area): void {
    let data = msg.data as string;
    switch (msg.type) {
      case 'console': {
        homeArea.addConsole(data);
        break;
      }
      case 'internal_error':
        data += ' ' + Strings.INTERNAL_ERROR_MESSAGE;
        // Intentional: fall through
      case 'stderr':
      case 'stdout': {
        // Split multiline messages into single lines for processing
        const outMsgList = data.split(/\r?\n/);
        for (const outMsg of outMsgList) {
          const ctRegex = /^([a-zA-Z._0-9-]+):(\d+):(\d+):(.+)$/m;
          const rtRegex = /^raised .+ : ([a-zA-Z._0-9-]+):(\d+) (.+)$/m;
          const ctMatchFound = outMsg.match(ctRegex);
          const rtMatchFound = outMsg.match(rtRegex);
          // Lines that contain a sloc are clickable:
          const cb = (basename: string, row: number, col: number,
              view: EditorView): void => {
            if (window.getSelection()?.toString() == '') {
              view.header.scrollIntoView(true);
              view.header.click();
              // Jump to corresponding line
              view.editor.gotoLine(basename, row, col);
            }
          };

          if (ctMatchFound?.length == 5) {
            const basename = ctMatchFound[1];
            const view = this.viewMap.get(basename);
            const row = parseInt(ctMatchFound[2]);
            const col = parseInt(ctMatchFound[3]);

            if (!view) {
              throw Error('Basename not found: ' + basename);
            }

            if (ctMatchFound[4].indexOf(' info:') === 0) {
              homeArea.addInfo(outMsg, () => {
                cb(basename, row, col, view);
              });
              view.editor.setGutterAnnotation(basename, row, col, outMsg,
                  'info');
            } else {
              if (ctMatchFound[4].indexOf(' warning:') === 0) {
                view.editor.setGutterAnnotation(basename, row, col, outMsg,
                    'warning');
              } else {
                view.editor.setGutterAnnotation(basename, row, col, outMsg,
                    'error');
              }
              homeArea.addMsg(outMsg, () => {
                cb(basename, row, col, view);
              });
            }
          } else if (rtMatchFound?.length == 4) {
            const basename = rtMatchFound[1];
            const view = this.viewMap.get(basename);
            const row = parseInt(rtMatchFound[2]);
            const col = 1;

            if (!view) {
              throw Error('Basename not found: ' + basename);
            }

            homeArea.addMsg(outMsg, () => {
              cb(basename, row, col, view);
            });
            view.editor.setGutterAnnotation(basename, row, col, outMsg,
                'error');
          } else {
            homeArea.addLine(outMsg);
          }
        }
        break;
      }
      default: {
        homeArea.addLine(data);
        throw new Error('Unhandled msg type.');
      }
    }
  }

  /**
   * Process the output from "check_output" ajax request
   * @param {CheckOutput.FS} data - The data from check_output
   * @return {number} the number of lines read by this function
   */
  private processCheckOutput(data: CheckOutput.FS): boolean {
    const homeArea = this.getHomeArea(data);
    for (const msg of data.output) {
      this.handleMsgType(msg, homeArea);
    }

    if (data.completed) {
      if (data.status != 0) {
        this.outputArea.addError(Strings.EXIT_STATUS_LABEL +
            ': ' + data.status);
      }
    }

    return data.completed;
  }

  /**
   * Reset the editors, outputArea
   */
  protected resetEditors(): void {
    this.outputArea.reset();

    for (const t of this.viewMap.values()) {
      t.editor.reset();
    }
  }
}

/**
 * The LabWidget class
 * @extends Widget
 */
export class LabWidget extends Widget {
  private readonly labContainer: LabContainer;

  /**
   * Creates an instance of LabWidget and attaches logic to DOM
   *
   * @param {HTMLDivElement} elem - the container for the widget
   * @param {(EditorMap | undefined)} dep - The view of widgets with the same
   *  name on the current page
   */
  constructor(elem: HTMLDivElement, dep: EditorMap | undefined) {
    super(elem, dep);
    const labArea = this.getElem('lab-area') as HTMLDivElement;
    this.labContainer = new LabContainer(labArea);
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
   * @param {CheckOutput.FS} data - if not null, the lab ref
   * @return {Area} the area to place returned data
   */
  protected getHomeArea(data: CheckOutput.FS): Area {
    if (data.ref !== undefined) {
      return this.labContainer.getLabArea(data.ref);
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
          this.labContainer.processResults(msg.data as CheckOutput.LabOutput);
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
}

type WidgetMap = Map<string, EditorMap>
type WidgetLike = Widget | LabWidget;
/**
 * Entrypoint for widget creation
 *
 * @export
 * @param {Array<HTMLDivElement>} widgets - The collection of widgets
 *    found on the page
 */
export function widgetFactory(widgets: Array<HTMLDivElement>): void {
  const widgetList: WidgetMap = new Map();

  for (const element of widgets) {
    try {
      let widget: WidgetLike;
      // Get data from element
      const name = element.dataset.name as string;
      const lab = element.dataset.lab as string;
      const depList = widgetList.get(name);

      if (lab === 'True') {
        widget = new LabWidget(element, depList);
      } else {
        widget = new Widget(element, depList);
      }

      // reset the view for this widget with the newly computed view
      widgetList.set(name, widget.viewMap);
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
}
