import ace from 'brace';
import 'brace/mode/ada';
import 'brace/mode/c_cpp';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

import {Resource} from './resource';

export enum EditorTheme {
  Light = 'ace/theme/tomorrow',
  Dark = 'ace/theme/tomorrow_night'
}

export enum EditorLanguage {
  Ada = 'ace/mode/ada',
  C_CPP = 'ace/mode/c_cpp'
}

/** Class representing an Editor **/
export class Editor {
  private readonly container: HTMLDivElement;
  private editor: ace.Editor;
  private readonly initialContents: string;
  private readonly basename: string;
  private tab: HTMLButtonElement | null = null;

  /**
   * Create an Editor
   * @param {Resource} resource - The resource to load into the editor
   */
  constructor(resource: Resource) {
    this.container = document.createElement('div');
    this.container.classList.add('editor-container');

    this.editor = ace.edit(this.container);

    // Set the mode
    if (resource.basename?.match(/.ad[sb]$/)) {
      this.editor.session.setMode(EditorLanguage.Ada);
    } else {
      this.editor.session.setMode(EditorLanguage.C_CPP);
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
      theme: EditorTheme.Light,
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
   *  Method to destruct the object. Used primarily for testing.
   */
  public destructor(): void {
    this.editor.destroy();
  }

  /**
   * Set the length of the visible lines in the editor
   * @param {number} length - The number of visible lines in the editor
   */
  public setLength(length: number): void {
    this.editor.setOption('minLines', length);
    this.editor.resize();
  }

  /**
   * Get the length of the visible lines in the editor
   * @return {number} - The number of visible rows in the editor
   */
  public getLength(): number {
    const maxLength = this.editor.getOption('maxLines');
    const length = this.editor.session.doc.getLength();

    return (length > maxLength) ? maxLength : length;
  }

  /**
   * Set the theme of the editor
   * @param {EditorTheme} theme - The ace theme to load
   */
  public setTheme(theme: EditorTheme): void {
    this.editor.setTheme(theme);
  }

  /**
   * Reset the editor back to default state
   */
  public reset(): void {
    this.editor.setValue(this.initialContents);
    this.editor.gotoLine(1);
    this.clearGutterAnnotation();
  }

  /**
   * Render the editor
   * @return {HTMLDivElement} The HTMLDivElement object holding the editor
   */
  public render(): HTMLDivElement {
    return this.container;
  }

  /**
   * Get the resource from the Editor
   * @return {esource} The Editor filename and contents
   */
  public getResource(): Resource {
    return {basename: this.basename, contents: this.editor.getValue()};
  }

  /**
   * Store the tab holding this editor
   * @param {HTMLButtonElement} tab - The tab holding this editor
   */
  public setTab(tab: HTMLButtonElement): void {
    this.tab = tab;
  }

  /**
   * Return the tab holding this editor
   * @return {HTMLButtonElement} The tab holding this editor
   */
  public getTab(): HTMLButtonElement | null {
    return this.tab;
  }

  /**
   * Jump the editor to row:col
   * @param {number} line - The line number to goto
   * @param {number} col - The col + 1 to goto
   */
  public gotoLine(line: number, col: number): void {
    this.editor.gotoLine(line, col - 1, true);
    this.editor.focus();
  }

  /**
   * Add gutter annotations to the ace editor session
   *
   * @param {number} line - The line number
   * @param {number} col - The column number
   * @param {string} msg - The corresponding message
   * @param {string} type - The type of annotation
   */
  public setGutterAnnotation(line: number, col: number, msg: string,
      type: string): void {
    const session = this.editor.getSession();
    const oldAnnotations = session.getAnnotations();
    const newAnnotation = {
      row: line - 1,
      column: col,
      text: msg,
      type: type,
    };
    session.setAnnotations([...oldAnnotations, newAnnotation]);
  }

  /**
   *  Clear the annotations in the gutter
   */
  public clearGutterAnnotation(): void {
    this.editor.getSession().clearAnnotations();
  }
}
