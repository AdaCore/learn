// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';

chai.use(chaiDom);

import ace from 'brace';

import {Editor, EditorTheme} from '../../src/ts/editor.ts';
import {Resource} from '../../src/ts/resource.ts';

describe('Editor', () => {
  let inTest: Editor;
  let parent: HTMLDivElement;
  let editor: ace.Editor;

  const resource: Resource = {
    basename: 'my_file.adb',
    contents: 'This is an adb file.',
  };

  before(() => {
    parent = document.createElement('div');
    inTest = new Editor(parent);
    editor = ace.edit(parent);
  });

  after(() => {
    inTest.destructor();
    inTest = null;
    parent = null;
  });

  describe('#addSession(), #setSession()', () => {
    it('should add a session with an adb file', () => {
      inTest.addSession(resource.basename, resource.contents);
      inTest.setSession(resource.basename);
      const session = editor.getSession();
      expect(session.getValue()).to.equal(resource.contents);
    });
  });

  describe('#getSessionContent()', () => {
    it('should return the session contents', () => {
      const sessionContent = inTest.getSessionContent(resource.basename);
      expect(sessionContent).to.equal(resource.contents);
    });

    it('should throw an exception for invalid name query', () => {
      const basename = 'Unknown.txt';
      expect(() => {
        inTest.getSessionContent(basename);
      }).to.throw('Unknown session with name ' + basename);
    });
  });

  describe('#setTheme()', () => {
    it('should start with the light theme', () => {
      expect(editor.getTheme()).to.equal(EditorTheme.Light);
    });

    it('should set the theme to dark', () => {
      inTest.setTheme(EditorTheme.Dark);
      expect(editor.getTheme()).to.equal(EditorTheme.Dark);
    });

    it('should set the theme to light', () => {
      inTest.setTheme(EditorTheme.Light);
      expect(editor.getTheme()).to.equal(EditorTheme.Light);
    });
  });

  describe('#reset()', () => {
    it('should modify the initial contents and move the cursor', () => {
      editor.getSession().doc.insert({row: 0, column: 0}, '\n');
      const row = editor.session.getLength() - 1;
      const column = editor.session.getLine(row).length;
      editor.gotoLine(row + 1, column);

      expect(editor.getValue()).not.to.equal(resource.contents);
      expect(editor.getCursorPosition()).not.to.deep.equal({row: 0, column: 0});
    });

    it('should reset the content and the cursor', () => {
      inTest.reset();
      expect(editor.getValue()).to.equal(resource.contents);
      expect(editor.getCursorPosition()).to.deep.equal({row: 0, column: 0});
    });
  });

  describe('#gotoLine', () => {
    it('should start with the cursor at 0, 0', () => {
      expect(editor.getCursorPosition()).to.deep.equal({row: 0, column: 0});
    });

    it('should jump to a new location', () => {
      inTest.gotoLine(resource.basename, 1, 2);
      expect(editor.getCursorPosition()).to.deep.equal({row: 0, column: 1});
    });
  });

  describe('#setGutterAnnotation()', () => {
    it('should set the gutter annotation', () => {
      inTest.setGutterAnnotation(resource.basename, 1, 1, 'test', 'error');

      const annotations = editor.getSession().getAnnotations();
      expect(annotations[0]).to.deep.equal({
        row: 0,
        column: 1,
        text: 'test',
        type: 'error',
      });
    });
  });
});
