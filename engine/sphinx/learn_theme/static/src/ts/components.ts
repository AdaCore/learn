export class Tabs {
  private headers : JQuery[];
  private contents : JQuery[];

  public addTab(name : string, content : JQuery)  : JQuery {
    this.contents.push($('<div>')
        .addClass('tab-content')
        .append(content));

    const header = $('<button>')
        .addClass('tab-links')
        .click((event: JQuery.Event) => {
          for(const c of this.contents) {
            c.hide();
            c.removeClass('active');
          }
          for(const h of this.headers) {
            h.removeClass('active');
          }

          contentContainer.addClass('active');
          contentContainer.show();
          event.target.addClass('active');
        });
    this.headers.push(header);
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

function Button(classList : string[], title : string, text : string) : JQuery {
  const button = $('<button>')
          .attr('type', 'button')
          .addClass('btn')
          .addClass('btn-primary')
          .attr('title', title)
          .text(text);
  for(let c of classList) {
    button.addClass(c);
  }
  return button;
}
