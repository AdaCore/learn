export class Tabs {
  private headers : Array<JQuery> = [];
  private contents : Array<JQuery> = [];

  public addTab(name : string, content : JQuery)  : JQuery {
    const tabContent = $('<div>')
        .addClass('tab-content')
        .append(content);

    const header = $('<button>')
        .addClass('tab-links')
        .text(name)
        .click(() => {
          for(const c of this.contents) {
            c.hide();
            c.removeClass('active');
          }
          for(const h of this.headers) {
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

  public render(parent : JQuery) {
    const header_container = $('<div>').addClass('tab').appendTo(parent);
    for(const h of this.headers) {
      h.appendTo(header_container);
    }
    for(const c of this.contents) {
      c.appendTo(parent);
    }
    if(this.headers.length > 0) {
      this.headers[0].click();
    }
  }

  public show(show : boolean) {
    if (show) {
      for(const h of this.headers) {
        h.show();
      }
      for(const c of this.contents) {
        if(c.hasClass('active')) {
          c.show();
        } else {
          c.hide();
        }
      }
    } else {
      for(const h of this.headers) {
        h.hide();
      }
      for(const c of this.contents) {
        c.show();
      }
    }
  }
}

export class Button {
  private obj : JQuery;
  public disabled = false;

  constructor(classList : string[], title : string, text : string){
    this.obj = $('<button>')
        .attr('type', 'button')
        .addClass('btn')
        .addClass('btn-primary')
        .attr('title', title)
        .text(text);
    for(let c of classList) {
      this.obj.addClass(c);
    }
  }

  public render() {
    return this.obj;
  }
}

export class CheckBox {
  private container : JQuery;
  private input : JQuery;
  private label : JQuery;
  private state : boolean;

  constructor(label : string, parent? : JQuery, classes? : string[], title? : string) {
    if(parent == undefined) {
      this.container = $('<div>');
    } else {
      this.container = parent;
    }

    if(classes != undefined) {
      for(const c in classes) {
        this.container.addClass(c);
      }
    }

    const qId = this.generateUniqueId();
    this.input = $('<input>')
        .attr('type', 'checkbox')
        .attr('id', qId)
        .appendTo(this.container);
    if(title != undefined) {
      this.input.attr('title', title);
    }

    this.label = $('<label>')
        .attr('for', qId)
        .text(label)
        .appendTo(this.container);
  }

  public checked() : boolean {
    return this.input.is(':checked');
  }

  public getCheckBox() : JQuery {
    return this.input;
  }

  public render() : JQuery {
    return this.container;
  }

  private generateUniqueId() {
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
