// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';

chai.use(chaiDom);

import ace from 'brace';

import {Editor, EditorLanguage, EditorTheme} from '../../src/ts/editor';
import {Resource} from '../../src/ts/types';

describe('Editor', () => {
  describe('#constructor()', () => {
    let counter = 0;
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resourceList: Array<Resource> = [
      {
        basename: 'my_file.adb',
        contents: 'This is an adb file.',
      },
      {
        basename: 'my_file.ads',
        contents: 'This is an ads file.',
      },
      {
        basename: 'my_file.cpp',
        contents: 'This is an c/cpp file.',
      },
    ];

    beforeEach(() => {
      inTest = new Editor(resourceList[counter]);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    afterEach(() => {
      counter++;
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should create an editor with an adb file', () => {
      expect(parent).to.have.tagName('div');
      expect(parent).to.have.class('editor-container');

      expect(editor.getOption('mode')).to.equal(EditorLanguage.Ada);
      expect(editor.getValue()).to.equal(resourceList[counter].contents);
    });

    it('should create an editor with an ads file', () => {
      expect(editor.getOption('mode')).to.equal(EditorLanguage.Ada);
      expect(editor.getValue()).to.equal(resourceList[counter].contents);
    });

    it('should create an editor with an c file', () => {
      expect(editor.getOption('mode')).to.equal(EditorLanguage.C_CPP);
      expect(editor.getValue()).to.equal(resourceList[counter].contents);
    });
  });

  describe('#setLength()', () => {
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents',
    };

    beforeEach(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    afterEach(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should start with length 1', () => {
      expect(editor.session.getLength()).to.equal(1);
    });

    it('should increase to length after set', () => {
      inTest.setLength(20);
      expect(editor.getOption('minLines')).to.equal(20);
      // TODO: check the resize happened
    });
  });

  describe('#getLength()', () => {
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents',
    };

    beforeEach(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    afterEach(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should return the length of the editor', () => {
      expect(inTest.getLength()).to.equal(1);
    });

    it('should return maxlines if length > maxlines', () => {
      const maxLines = editor.getOption('maxLines');
      for (let i = 0; i < maxLines - 2; i++) {
        editor.session.doc.insert({row: 0, column: 0}, '\n');
      }
      expect(inTest.getLength()).to.equal(maxLines - 1);

      for (let i = 0; i < 10; i++) {
        editor.session.doc.insert({row: 0, column: 0}, '\n');
      }

      expect(inTest.getLength()).to.equal(maxLines);
    });
  });

  describe('#setTheme()', () => {
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents',
    };

    before(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    after(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

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
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents',
    };

    before(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    after(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should modify the initial contents and move the cursor', () => {
      editor.session.doc.insert({row: 0, column: 0}, '\n');
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

  describe('#getResource()', () => {
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents',
    };

    before(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    after(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should return the original resource', () => {
      const newResource = inTest.getResource();
      expect(newResource).to.deep.equal(resource);
    });

    it('should return the edited resource', () => {
      editor.session.doc.insert({row: 0, column: 0}, '++');
      const newResource = inTest.getResource();
      expect(newResource).to.deep.equal({
        basename: resource.basename,
        contents: '++' + resource.contents,
      });
    });
  });

  describe('#setTab() and #getTab()', () => {
    let inTest: Editor;
    let parent: HTMLDivElement;

    // eslint-disable-next-line no-unused-vars,@typescript-eslint/no-unused-vars
    let editor: ace.Editor;
    let tab: HTMLButtonElement;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents',
    };

    before(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
      tab = document.createElement('button');
    });

    after(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should set and get the tab', () => {
      inTest.setTab(tab);
      expect(inTest.getTab()).to.equal(tab);
    });
  });

  describe('#gotoLine', () => {
    let inTest: Editor;
    let parent: HTMLElement;
    let editor: ace.Editor;

    const resource: Resource = {
      basename: 'file.adb',
      contents: 'file contents\nmore contents',
    };

    before(() => {
      inTest = new Editor(resource);
      parent = inTest.render();
      editor = ace.edit(parent);
    });

    after(() => {
      inTest.destructor();
      inTest = null;
      parent = null;
    });

    it('should start with the cursor at 0, 0', () => {
      expect(editor.getCursorPosition()).to.deep.equal({row: 0, column: 0});
    });

    it('should jump to the second row, second column', () => {
      inTest.gotoLine(2, 2);
      expect(editor.getCursorPosition()).to.deep.equal({row: 1, column: 1});
    });
  });
});
