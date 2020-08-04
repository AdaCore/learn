// Import testing libs
import chai, {expect} from 'chai';
chai.use(require('chai-dom'));
chai.use(require('chai-as-promised'));

import fetchMock from 'fetch-mock';

import {widgetFactory, Widget, LabWidget} from '../../src/ts/widget';

describe('widgetFactory()', () => {
  let htmlList: HTMLCollectionOf<Element>;
  let inTestList: Array<Widget | LabWidget>;
  let pageWidget: HTMLElement;
  let file: HTMLElement;

  beforeEach(() => {
    pageWidget = document.createElement('div');
    pageWidget.classList.add('widget_editor');
    pageWidget.setAttribute('example_server', 'http://example.com');
    pageWidget.setAttribute('run_button', 'True');
    pageWidget.setAttribute('inline', 'True');
    document.body.appendChild(pageWidget);

    const shadowFile = document.createElement('div');
    shadowFile.classList.add('shadow_file');
    shadowFile.setAttribute('basename', 'shadow.txt');
    shadowFile.textContent = 'Hello shadow';
    pageWidget.appendChild(shadowFile);

    file = document.createElement('div');
    file.classList.add('file');
    file.setAttribute('basename', 'test.adb');
    file.textContent = 'Hello world';
    pageWidget.appendChild(file);
  });

  afterEach(() => {
    for(let inTest of inTestList) {
      inTest.destructor();
    }

    document.body.innerHTML = '';
  });

  it('should be a list with 2 elements', () => {
    const labWidget = document.createElement('div');
    labWidget.classList.add('widget_editor');
    labWidget.setAttribute('example_server', 'http://example.com');
    labWidget.setAttribute('run_button', 'True');
    labWidget.setAttribute('inline', 'True');
    labWidget.setAttribute('lab', 'True');
    document.body.appendChild(labWidget);
    labWidget.appendChild(file);

    htmlList = document.getElementsByClassName('widget_editor')
    inTestList = widgetFactory(htmlList);

    expect(inTestList).to.have.lengthOf(2);
    expect(inTestList[0]).to.be.an.instanceof(Widget);
    expect(inTestList[1]).to.be.an.instanceof(LabWidget);
  });

  it('should be throw an exception with malformed widget', () => {
    pageWidget.removeAttribute('example_server');
    htmlList = document.getElementsByClassName('widget_editor')
    const fn = () => {
      inTestList = widgetFactory(htmlList);
    };

    expect(fn).to.throw(Error, 'Malformed widget! No server address specified.');
  });
});

describe('Widget', () => {
  const baseURL = 'http://example.com'
  let htmlList: HTMLCollectionOf<Element>;
  let inTestList: Array<Widget | LabWidget>;

  const pageWidget = document.createElement('div');
  pageWidget.classList.add('widget_editor');
  pageWidget.setAttribute('example_server', baseURL);
  pageWidget.setAttribute('name', 'pageWidget');
  pageWidget.setAttribute('run_button', 'True');
  pageWidget.setAttribute('inline', 'True');

  const file = document.createElement('div');
  file.classList.add('file');
  file.setAttribute('basename', 'test.adb');
  file.textContent = 'Hello world';
  pageWidget.appendChild(file);

  beforeEach(() => {
    document.body.appendChild(pageWidget);
    htmlList = document.getElementsByClassName('widget_editor')
    inTestList = widgetFactory(htmlList);
  });

  afterEach(() => {
    for(let inTest of inTestList) {
      inTest.destructor();
    }

    document.body.innerHTML = '';
  });

  describe('test run button click', () => {

    afterEach(() => {
      fetchMock.reset();
    });

    it('should trigger a run sequence when clicked', async () => {
      fetchMock.post(baseURL + '/run_program/', {
        body: {
          'identifier': '1',
          'message' : 'Pending',
        },
      });
      fetchMock.post(baseURL + '/check_output/', {
        body: {
          'completed': false,
          'message': 'PENDING',
          'output': [],
          'status': 0
        },
      }, {
        repeat: 1,
      });
      fetchMock.post(baseURL + '/check_output/', {
        body: {
          'completed': false,
          'message':'PENDING',
          'output': [
            {
              'msg': {
                'data': 'This is a console message',
                'type': 'console'
              }
            }
          ],
          'status': 0
        },
      }, {
        repeat: 1,
        overwriteRoutes: false,
      });
      fetchMock.post(baseURL + '/check_output/', {
        body: {
          'completed': false,
          'message': 'PENDING',
          'output': [
            {
              'msg':{
                'data': 'First stdout message',
                'type': 'stdout'
              }
            }, {
              'msg': {
                'data': 'Second stdout message',
                'type': 'stdout'
              }
            }
          ],
          'status': 0
        },
      }, {
        repeat: 1,
        overwriteRoutes: false,
      });
      fetchMock.post(baseURL + '/check_output/', {
        body: {
          'completed': true,
          'message': 'SUCCESS',
          'output': [],
          'status': 0
        },
      }, {
        repeat: 1,
        overwriteRoutes: false,
      });

      expect(pageWidget).to.have.descendants('div.output_row').and.have.length(1);
      const outputRow = pageWidget.querySelector('div.output_row');
      expect(outputRow).to.have.descendants('button').and.have.length(1);
      const button = outputRow.querySelector('button');

      button.click();
      await fetchMock.flush(true);
      const runProgram = fetchMock.calls(baseURL + '/run_program/');
      expect(runProgram).to.have.length(1);
      expect(runProgram[0][1]['body']).to.equal(JSON.stringify({
        'files': [
          {
            'basename': 'test.adb',
            'contents': 'Hello world',
          }
        ],
        'mode': 'run',
        'name': 'pageWidget',
        'lab': false,
      }));
    });
  });

});