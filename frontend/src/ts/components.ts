/** Class represents Tab component */
export class Tabs {
  private headers: Array<HTMLButtonElement> = [];
  private contents: Array<HTMLDivElement> = [];

  /**
   * The event callback signature for tabs
   *
   * @callback tabChange
   */

  /**
   * Add a Tab
   * @param {string} name - The name to put at the tab button
   * @param {HTMLDivElement} content - The content to put inside the tab
   * @param {tabChange} fn - The function to call when the tab changes
   * @return {HTMLDivElement} The button or header for the new tab
   */
  public addTab(name: string, content: HTMLDivElement,
      fn: () => void): HTMLButtonElement {
    const tabContent = document.createElement('div');
    tabContent.classList.add('tab-content');
    tabContent.appendChild(content);

    const header = document.createElement('button');
    header.classList.add('tab-links');
    header.textContent = name;
    header.addEventListener('click', () => {
      for (const c of this.contents) {
        c.style.display = 'none';
        c.classList.remove('active');
      }
      for (const h of this.headers) {
        h.classList.remove('active');
      }

      fn();

      tabContent.classList.add('active');
      tabContent.style.display = 'block';
      header.classList.add('active');
    });

    this.headers.push(header);
    this.contents.push(tabContent);
    return header;
  }

  /**
   * Render the tab into the parent
   * @param {HTMDivLElement} parent - The parent to put the tab into
   */
  public render(parent: HTMLDivElement): void {
    const headerContainer = document.createElement('div');
    headerContainer.classList.add('tab');
    parent.appendChild(headerContainer);

    for (const h of this.headers) {
      headerContainer.appendChild(h);
    }
    for (const c of this.contents) {
      parent.appendChild(c);
    }
    /* istanbul ignore next */
    if (this.headers.length > 0) {
      this.headers[0].click();
    }
  }

  /**
   * Enable or disable the tabs
   * @param {boolean} show - True for show, false for hide
   */
  public show(show: boolean): void {
    if (show) {
      for (const h of this.headers) {
        h.style.display = 'block';
      }
      for (const c of this.contents) {
        if (c.classList.contains('active')) {
          c.style.display = 'block';
        } else {
          c.style.display = 'none';
        }
      }
    } else {
      for (const h of this.headers) {
        h.style.display = 'none';
      }
      for (const c of this.contents) {
        c.style.display = 'block';
      }
    }
  }
}

/** Class represents a Button */
export class Button {
  private readonly obj: HTMLButtonElement;
  public disabled = false;

  /**
   * Constructs a button
   * @param {string[]} classList - The list of classes to apply to the button
   * @param {string} title - The title to put on the button for tooltip
   * @param {string} text - The text to display on the button
   */
  constructor(classList: Array<string>, title: string, text: string) {
    this.obj = document.createElement('button');
    this.obj.setAttribute('type', 'button');
    this.obj.classList.add('btn', 'btn-primary', ...classList);
    this.obj.setAttribute('title', title);
    this.obj.textContent = text;
  }

  /**
   * The event callback signature for buttons
   *
   * @callback eventCallback
   * @param {Event} event - The event that triggered the call
   */

  /**
   * Registers an event on the button
   * @param {string} type - The event type to register ["click", "focus", etc]
   * @param {eventCallback} fn - The callback to trigger
   */
  public registerEvent(type: string,
      fn: (event: Event) => void): void {
    this.obj.addEventListener(type, fn);
  }

  /**
   * Render the button
   * @return {HTMLButtonElement} The button
   */
  public render(): HTMLButtonElement {
    return this.obj;
  }
}

/** Class represents a Group of Buttons */
export class ButtonGroup {
  private btnList: Array<Button> = [];
  private enabled = true;

  /**
   * Adds a button to the button group
   * @param {string[]} classList - The list of classes to apply to the button
   * @param {string} title - The title to put on the button for tooltip
   * @param {string} text - The text to display on the button
   * @param {string} type - The type of callback event to register
   * @param {eventCallback} fn - The function to register the event for
   */
  public addButton(classList: Array<string>, title: string, text: string,
      type: string, fn: () => void): void {
    const btn: Button = new Button(classList, title, text);
    this.btnList.push(btn);

    btn.registerEvent(type, async () => {
      if (this.enabled) {
        this.disable();
        await fn();
        this.enable();
      }
    });
  }

  /**
   * Return the number of buttons in the group
   * @return {number} the number of buttons in the group
   */
  public length(): number {
    return this.btnList.length;
  }

  /**
   * Disable the buttons in the group
   */
  private disable(): void {
    this.enabled = false;
  }

  /**
   * Enable the buttons in the group
   */
  private enable(): void {
    this.enabled = true;
  }

  /**
   * Render the button group
   * @return {HTMLDivElement} - The rendered elements
   */
  public render(): HTMLDivElement {
    const elem = document.createElement('div');
    elem.classList.add('col-md-3');

    for (const b of this.btnList) {
      elem.appendChild(b.render());
    }

    return elem;
  }
}

/** Class represents a checkbox */
export class CheckBox {
  private readonly container: HTMLDivElement;
  private readonly input: HTMLInputElement;
  private label: HTMLLabelElement;
  // private state: boolean;

  /**
   * Construct a checkbox
   * @param {string} label - The label text
   * @param {HTMLDivElement} [parent] - The parent to insert the checkbox into
   * @param {string[]} [classes] - The classes to apply to the checkbox
   * @param {string} [title] - The title for the checkbox, tooltip
   */
  constructor(label: string,
      parent? : HTMLDivElement,
      classes? : string[],
      title? : string) {
    if (parent == undefined) {
      this.container = document.createElement('div');
    } else {
      this.container = parent;
    }

    if (classes != undefined) {
      for (const c of classes) {
        this.container.classList.add(c);
      }
    }

    const qId = this.generateUniqueId();
    this.input = document.createElement('input');
    this.input.setAttribute('type', 'checkbox');
    this.input.setAttribute('id', qId);
    this.input.classList.add('checkbox');
    this.container.appendChild(this.input);

    if (title != undefined) {
      this.input.setAttribute('title', title);
    }

    this.label = document.createElement('label');
    this.label.setAttribute('for', qId);
    this.label.textContent = label;
    this.container.appendChild(this.label);
  }

  /**
   * Gets whether the checkbox is checked
   * @return {boolean} True for checked
   */
  public checked(): boolean {
    return this.input.checked;
  }

  /**
   * Returns the actual checkbox HTMLInputElement object
   * @return {HTMLInputElement} The checkbox
   */
  public getCheckBox(): HTMLInputElement {
    return this.input;
  }

  /**
   * Renders the checkbox
   * @return {HTMLDivElement} The container with the label and checkbox
   */
  public render(): HTMLDivElement {
    return this.container;
  }

  /**
   * Generates a unique ID
   * @return {string} A unique ID
   */
  private generateUniqueId(): string {
    let dt = new Date().getTime();
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
        .replace(/[xy]/g, function(c) {
          const r = (dt + Math.random()*16)%16 | 0;
          dt = Math.floor(dt/16);
          return (c=='x' ? r :(r&0x3|0x8)).toString(16);
        });
  }
}
