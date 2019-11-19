
/** Class represents Tab component */
export class Tabs {
  private headers: Array<JQuery> = [];
  private contents: Array<JQuery> = [];

  /**
   * Add a Tab
   * @param {string} name - The name to put at the tab button
   * @param {JQuery} content - The content to put inside the tab
   * @return {JQuery} The button or header for the new tab
   */
  public addTab(name: string, content: JQuery): JQuery {
    const tabContent = $('<div>')
        .addClass('tab-content')
        .append(content);

    const header = $('<button>')
        .addClass('tab-links')
        .text(name)
        .click(() => {
          for (const c of this.contents) {
            c.hide();
            c.removeClass('active');
          }
          for (const h of this.headers) {
            h.removeClass('active');
          }

          tabContent.addClass('active');
          tabContent.show();
          header.addClass('active');
        });
    this.headers.push(header);
    this.contents.push(tabContent);
    return header;
  }

  /**
   * Render the tab into the parent
   * @param {JQuery} parent - The parent to put the tab into
   */
  public render(parent: JQuery): void {
    const headerContainer = $('<div>').addClass('tab').appendTo(parent);
    for (const h of this.headers) {
      h.appendTo(headerContainer);
    }
    for (const c of this.contents) {
      c.appendTo(parent);
    }
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
        h.show();
      }
      for (const c of this.contents) {
        if (c.hasClass('active')) {
          c.show();
        } else {
          c.hide();
        }
      }
    } else {
      for (const h of this.headers) {
        h.hide();
      }
      for (const c of this.contents) {
        c.show();
      }
    }
  }
}

/** Class represents a Button */
export class Button {
  private obj: JQuery;
  public disabled = false;

  /**
   * Constructs a button
   * @param {string[]} classList - The list of classes to apply to the button
   * @param {string} title - The title to put on the button for tooltip
   * @param {string} text - The text to display on the button
   */
  constructor(classList: string[], title: string, text: string) {
    this.obj = $('<button>')
        .attr('type', 'button')
        .addClass('btn')
        .addClass('btn-primary')
        .attr('title', title)
        .text(text);
    for (const c of classList) {
      this.obj.addClass(c);
    }
  }

  /**
   * Render the button
   * @return {JQuery} The button
   */
  public render(): JQuery {
    return this.obj;
  }
}

/** Class represents a checkbox */
export class CheckBox {
  private container: JQuery;
  private input: JQuery;
  private label: JQuery;
  private state: boolean;

  /**
   * Construct a checkbox
   * @param {string} label - The label text
   * @param {JQuery} [parent] - The parent to insert the checkbox into
   * @param {string[]} [classes] - The classes to apply to the checkbox
   * @param {string} [title] - The title for the checkbox, tooltip
   */
  constructor(label: string,
      parent? : JQuery,
      classes? : string[],
      title? : string) {
    if (parent == undefined) {
      this.container = $('<div>');
    } else {
      this.container = parent;
    }

    if (classes != undefined) {
      for (const c of classes) {
        this.container.addClass(c);
      }
    }

    const qId = this.generateUniqueId();
    this.input = $('<input>')
        .attr('type', 'checkbox')
        .attr('id', qId)
        .addClass('checkbox')
        .appendTo(this.container);
    if (title != undefined) {
      this.input.attr('title', title);
    }

    this.label = $('<label>')
        .attr('for', qId)
        .text(label)
        .appendTo(this.container);
  }

  /**
   * Gets whether the checkbox is checked
   * @return {boolean} True for checked
   */
  public checked(): boolean {
    return this.input.is(':checked');
  }

  /**
   * Returns the actual checkbox JQuery object
   * @return {JQuery} The checkbox
   */
  public getCheckBox(): JQuery {
    return this.input;
  }

  /**
   * Renders the checkbox
   * @return {JQuery} The container with the label and checkbox
   */
  public render(): JQuery {
    return this.container;
  }

  /**
   * Generates a unique ID
   * @return {string} A unique ID
   */
  private generateUniqueId(): string {
    let dt = new Date().getTime();
    const uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
        .replace(/[xy]/g, function(c) {
          const r = (dt + Math.random()*16)%16 | 0;
          dt = Math.floor(dt/16);
          return (c=='x' ? r :(r&0x3|0x8)).toString(16);
        });
    return uuid;
  }
}
