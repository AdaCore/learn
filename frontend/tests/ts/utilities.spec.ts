// Import testing libs
import {expect} from 'chai';
import 'mocha';

// Import package under test
import * as util from 'ts/utilities';

describe('isString', () => {
  it('should return true for a string and false for other', () => {
    const trueResult = util.isString('Hello');
    expect(trueResult).to.be.true;

    const falseResult = util.isString(5);
    expect(falseResult).to.be.false;
  });
});

describe('isNumber', () => {
  it('should return true for a number and false for other', () => {
    const trueResult = util.isNumber(5);
    expect(trueResult).to.be.true;

    const falseResult = util.isNumber('Hello');
    expect(falseResult).to.be.false;
  });
});

describe('isArray', () => {
  it('should return true for an array and false for other', () => {
    const trueResult = util.isArray([1, 2, 3]);
    expect(trueResult).to.be.true;

    const falseResult = util.isArray(4);
    expect(falseResult).to.be.false;
  });
});

describe('isFunction', () => {
  it('should return true for a function and false for other', () => {
    const trueResult = util.isFunction(util.isFunction);
    expect(trueResult).to.be.true;

    const falseResult = util.isFunction(4);
    expect(falseResult).to.be.false;
  });
});

describe('isObject', () => {
  it('should return true for an object and false for other', () => {
    const trueResult = util.isObject({key: 'value'});
    expect(trueResult).to.be.true;

    const falseResult = util.isObject(4);
    expect(falseResult).to.be.false;
  });
});

describe('isNull', () => {
  it('should return true for null and false for other', () => {
    const trueResult = util.isNull(null);
    expect(trueResult).to.be.true;

    const falseResult = util.isNull(4);
    expect(falseResult).to.be.false;
  });
});

describe('isNull', () => {
  it('should return true for undefined and false for other', () => {
    const trueResult = util.isUndefined(undefined);
    expect(trueResult).to.be.true;

    const falseResult = util.isUndefined(4);
    expect(falseResult).to.be.false;
  });
});

describe('isUndefined', () => {
  it('should return true for undefined and false for other', () => {
    const trueResult = util.isUndefined(undefined);
    expect(trueResult).to.be.true;

    const falseResult = util.isUndefined(4);
    expect(falseResult).to.be.false;
  });
});

describe('isBoolean', () => {
  it('should return true for a boolean and false for other', () => {
    const trueResult = util.isBoolean(false);
    expect(trueResult).to.be.true;

    const falseResult = util.isBoolean(4);
    expect(falseResult).to.be.false;
  });
});

describe('isRegExp', () => {
  it('should return true for a regular expression and false for other', () => {
    const trueResult = util.isRegExp(/^[1-9]\d{0,2}$/g);
    expect(trueResult).to.be.true;

    const falseResult = util.isRegExp(4);
    expect(falseResult).to.be.false;
  });
});

describe('isError', () => {
  it('should return true for an error and false for other', () => {
    const trueResult = util.isError(Error('My Error'));
    expect(trueResult).to.be.true;

    const falseResult = util.isError(4);
    expect(falseResult).to.be.false;
  });
});

describe('isDate', () => {
  it('should return true for a date and false for other', () => {
    const trueResult = util.isDate(new Date());
    expect(trueResult).to.be.true;

    const falseResult = util.isDate(4);
    expect(falseResult).to.be.false;
  });
});

describe('isSymbol', () => {
  it('should return true for a symbol and false for other', () => {
    const trueResult = util.isSymbol(Symbol('key'));
    expect(trueResult).to.be.true;

    const falseResult = util.isSymbol(4);
    expect(falseResult).to.be.false;
  });
});
