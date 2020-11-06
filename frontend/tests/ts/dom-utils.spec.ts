// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';

chai.use(chaiDom);

import {getElemById, getElemAttr, getElemsByClass, getElemsByTag}
  from '../../src/ts/dom-utils';

describe('getElemById()', () => {
  const id = 'test-div';
  const content = 'Test';
  const inTest = document.createElement('div');
  inTest.setAttribute('id', id);
  inTest.textContent = content;
  document.body.appendChild(inTest);


  it('should return the element with my id', () => {
    const elem = getElemById(id);
    expect(elem).to.have.id(id);
    expect(elem).to.have.text(content);
  });

  it('should throw an error if an unknown id is queried', () => {
    const unknownId = 'unknown';
    expect(() => {
      getElemById(unknownId);
    }).to.throw('Malformed DOM. Cannot find elem ' + unknownId);
  });
});

describe('getElemAttr()', () => {
  const attr = 'test';
  const inTest = document.createElement('div');
  inTest.setAttribute('myAttr', attr);

  it('should return the attribute on the elem', () => {
    const a = getElemAttr(inTest, 'myAttr');
    expect(a).to.equal(attr);
  });

  it('should throw an error if an unknown attr is queried', () => {
    const unknownAttr = 'unknown';
    expect(() => {
      getElemAttr(inTest, unknownAttr);
    }).to.throw('Malformed elem. Cannot get attribute: ' + unknownAttr);
  });
});

describe('getElemsByClass', () => {
  const parent = document.createElement('div');
  const a = document.createElement('div');
  a.classList.add('test-class');
  parent.appendChild(a);
  const b = document.createElement('div');
  b.classList.add('test-class');
  parent.appendChild(b);
  const c = document.createElement('div');
  c.classList.add('diff-class');
  parent.appendChild(c);

  it('should return an array with the elems that have the class', () => {
    const elems = getElemsByClass(parent, 'test-class');
    expect(elems).to.have.length(2);
  });
});

describe('getElemsByTag', () => {
  const parent = document.createElement('div');
  const a = document.createElement('button');
  a.classList.add('test-class');
  parent.appendChild(a);
  const b = document.createElement('button');
  b.classList.add('test-class');
  parent.appendChild(b);
  const c = document.createElement('div');
  c.classList.add('diff-class');
  parent.appendChild(c);

  it('should return an array with the elems that have the class', () => {
    const elems = getElemsByTag(parent, 'button');
    expect(elems).to.have.length(2);
  });
});
