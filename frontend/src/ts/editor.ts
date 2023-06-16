import ace from 'brace';
import 'brace/mode/ada';
import 'brace/mode/c_cpp';
import 'brace/theme/tomorrow';
import 'brace/theme/tomorrow_night';

/* eslint-disable no-unused-vars */
export enum EditorTheme {
  Light = 'ace/theme/tomorrow',
  Dark = 'ace/theme/tomorrow_night'
}

export enum EditorLanguage {
  Ada = 'ace/mode/ada',
  C_CPP = 'ace/mode/c_cpp'
}
/* eslint-enable no-unused-vars */

interface SessionData {
  initialContents: string;
  session: ace.IEditSession;
}

type SessionMap = Map<string, SessionData>;

/** Class representing an Editor **/
export class Editor {
  private readonly editor: ace.Editor;
  private readonly nontabbedEditors : Array<ace.Editor>;

  private sessions: SessionMap = new Map();
  private maxLength = 0;

  /**
  * Creates an instance of Editor.
  *
  * @param {HTMLDivElement} elem - The element that will contain the editor
  */
  constructor(elem: HTMLDivElement) {
    this.editor = ace.edit(elem);
    this.initACEEditor(this.editor);
    this.nontabbedEditors = [];
  }

  /**
   *  Method to destruct the object. Used primarily for testing.
   */
  public destructor(): void {
    this.editor.destroy();
  }

  /**
  * Configures the ACE editor
  *
  * @param {ace.Editor} editor - The ACE editor
  */
  private initACEEditor(editor: ace.Editor) {
    editor.$blockScrolling = Infinity;

    // ... and content options
    editor.setShowPrintMargin(false);
    editor.gotoLine(1);

    editor.setOptions({
      highlightActiveLine: false,
      fontSize: 13,
      tabSize: 3,
      useSoftTabs: true,
      theme: EditorTheme.Light,
      minLines: editor.session.doc.getLength(),
      maxLines: 50,
    });

    // place the cursor at 1,1
    editor.selection.moveCursorTo(0, 0);

    editor.renderer.setScrollMargin(5, 5, 0, 0);
  }

  /**
   * Refresh editor sessions
   *
   * @param {boolean} isTabbed - Use tabbed session
   */
  public refresh(isTabbed: boolean): void {
    if (isTabbed) {
      this.editor.resize();
    } else {
      for (const e of this.nontabbedEditors) {
        e.resize();
      }
    }
  }

  /**
   * Add a session to the editor
   *
   * @param {string} basename - The name of the file
   * @param {string} content - The content of the session
   */
  public addSession(basename: string, content: string): void {
    const data: SessionData = {
      initialContents: content,
      session: new ace.EditSession(content),
    };

    // Set the mode
    if (basename.match(/.ad[sb]$/)) {
      data.session.setMode(EditorLanguage.Ada);
    } else {
      data.session.setMode(EditorLanguage.C_CPP);
    }

    // Resize the editor base on longest session
    const sessionLength = data.session.doc.getLength();
    if (sessionLength > this.maxLength) {
      this.maxLength = sessionLength;
      this.editor.setOption('minLines', this.maxLength);
      this.editor.resize();
    }

    // clear undo stack to avoid undoing everything we just did
    data.session.setUndoManager(new ace.UndoManager());

    this.sessions.set(basename, data);
  }

  /**
   * Add a session to the editor
   *
   * @param {string} basename - The name of the file
   * @param {HTMLDivElement} elem - The element that will contain the editor
   */
  public addNonTabbedEditor(basename: string, elem: HTMLDivElement): void {
    const data = this.getSession(basename);

    const newEditor = ace.edit(elem);
    this.initACEEditor(newEditor);
    newEditor.setSession(data.session);

    this.nontabbedEditors.push(newEditor);
  }

  /**
   * Helper function to get session and throw exception if not found
   *
   * @private
   * @param {string} basename - The session name to get
   * @return {SessionData} - The session data
   */
  private getSession(basename: string): SessionData {
    const session = this.sessions.get(basename);
    if (!session) {
      throw Error('Unknown session with name ' + basename);
    }

    return session;
  }

  /**
   * Change the editor active session to the one that corresponds to the
   * given basename
   *
   * @param {string} basename - The name of the session to change to
   */
  public setSession(basename: string): void {
    const session = this.getSession(basename);
    this.editor.setSession(session.session);
  }

  /**
   * Get the content of a session given a basename
   *
   * @param {string} basename - The name of the session to get
   * @return {string} - The content of the named session
   */
  public getSessionContent(basename: string): string {
    const session = this.getSession(basename);
    return session.session.getValue();
  }

  /**
   * Set the theme of the editor
   * @param {EditorTheme} theme - The ace theme to load
   */
  public setTheme(theme: EditorTheme): void {
    this.editor.setTheme(theme);

    for (const e of this.nontabbedEditors) {
      e.setTheme(theme);
    }
  }

  /**
   * Reset the editor back to default state
   */
  public reset(): void {
    for (const s of this.sessions.values()) {
      s.session.setValue(s.initialContents);
    }
    this.editor.gotoLine(1);
    this.clearGutterAnnotation();
  }

  /**
   * Jump the editor to basename session and row:col
   *
   * @param {string} basename - The name of the session to switch to
   * @param {number} line - The line number to goto
   * @param {number} col - The col + 1 to goto
   */
  public gotoLine(basename: string, line: number, col: number): void {
    this.setSession(basename);
    this.editor.gotoLine(line, col - 1, true);
    this.editor.focus();
  }

  /**
   * Add gutter annotations to the ace editor session
   *
   * @param {string} basename - The name of the session
   * @param {number} line - The line number
   * @param {number} col - The column number
   * @param {string} msg - The corresponding message
   * @param {string} type - The type of annotation
   */
  public setGutterAnnotation(basename: string, line: number, col: number,
      msg: string, type: string): void {
    const session = this.getSession(basename);
    const oldAnnotations = session.session.getAnnotations();
    const newAnnotation = {
      row: line - 1,
      column: col,
      text: msg,
      type: type,
    };
    session.session.setAnnotations([...oldAnnotations, newAnnotation]);
  }

  /**
   *  Clear the annotations in the gutter
   */
  public clearGutterAnnotation(): void {
    for (const s of this.sessions.values()) {
      s.session.clearAnnotations();
    }
  }
}
