import $ from 'jquery';
import * as ace from 'brace';
import 'brace/mode/ada';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

import * as Types from './types';

/** Class representing an Editor **/
export class Editor {
  private readonly container: JQuery;
  private editor: ace.Editor;
  private readonly initialContents: string;
  private readonly basename: string;
  private tab: JQuery;

  /**
   * Create an Editor
   * @param {Types.Resource} resource - The resource to load into the editor
   */
  constructor(resource: Types.Resource) {
    this.container = $('<div>').addClass('editor-container');
    this.editor = ace.edit(this.container[0]);

    // Set the mode
    if (resource.basename.match(/.ad[sb]$/)) {
      this.editor.session.setMode('ace/mode/ada');
    } else {
      this.editor.session.setMode('ace/mode/c_cpp');
    }
    this.editor.$blockScrolling = Infinity;

    // ... and their contents
    this.editor.setValue(resource.contents);
    this.editor.setShowPrintMargin(false);
    this.editor.gotoLine(1);

    this.initialContents = resource.contents;
    this.basename = resource.basename;

    this.editor.setOptions({
      highlightActiveLine: false,
      fontSize: 13,
      tabSize: 3,
      useSoftTabs: true,
      theme: 'ace/theme/tomorrow',
      minLines: this.editor.session.doc.getLength(),
      maxLines: 50,
    });

    this.editor.resize();
    // place the cursor at 1,1
    this.editor.selection.moveCursorTo(0, 0);

    // clear undo stack to avoid undoing everything we just did
    this.editor.getSession().setUndoManager(new ace.UndoManager());

    this.editor.renderer.setScrollMargin(5, 5, 0, 0);
  }

  /**
   * Set the theme of the editor
   * @param {string} theme - The ace theme to load
   */
  public setTheme(theme: string): void {
    this.editor.setTheme(theme);
  }

  /**
   * Reset the editor back to default state
   */
  public reset(): void {
    this.editor.setValue(this.initialContents);
    this.editor.gotoLine(1);
  }

  /**
   * Render the editor
   * @return {JQuery} The JQuery object holding the editor
   */
  public render(): JQuery {
    return this.container;
  }

  /**
   * Get the resource from the Editor
   * @return {Types.Resource} The Editor filename and contents
   */
  public getResource(): Types.Resource {
    return {basename: this.basename, contents: this.editor.getValue()};
  }

  /**
   * Store the tab holding this editor
   * @param {JQuery} tab - The tab holding this editor
   */
  public setTab(tab: JQuery): void {
    this.tab = tab;
  }

  /**
   * Return the tab holding this editor
   * @return {JQuery} The tab holding this editor
   */
  public getTab(): JQuery {
    return this.tab;
  }

  /**
   * Jumo the editor to row:col
   * @param {number} line - The line number to goto
   * @param {number} col - The col + 1 to goto
   */
  public gotoLine(line: number, col: number): void {
    this.editor.gotoLine(line, col - 1, true);
    this.editor.focus();
  }
}
