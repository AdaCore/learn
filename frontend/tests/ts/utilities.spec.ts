// Import testing libs
import { expect } from 'chai';
import 'mocha';

// Import package under test
import * as util from '../../src/ts/utilities.ts';

describe('isString', () => {

  it('should return true for a string and false for other', () => {
    const true_result = util.isString('Hello');
    expect(true_result).to.equal(true);

    const false_result = util.isString(5);
    expect(false_result).to.equal(false);
  });
});

describe('isNumber', () => {

  it('should return true for a number and false for other', () => {
    const true_result = util.isNumber(5);
    expect(true_result).to.equal(true);

    const false_result = util.isNumber('Hello');
    expect(false_result).to.equal(false);
  });
});

describe('isArray', () => {

  it('should return true for an array and false for other', () => {
    const true_result = util.isArray([1, 2, 3]);
    expect(true_result).to.equal(true);

    const false_result = util.isArray(4);
    expect(false_result).to.equal(false);
  });
});

describe('isFunction', () => {

  it('should return true for a function and false for other', () => {
    const true_result = util.isFunction(util.isFunction);
    expect(true_result).to.equal(true);

    const false_result = util.isFunction(4);
    expect(false_result).to.equal(false);
  });
});

describe('isObject', () => {

  it('should return true for an object and false for other', () => {
    const true_result = util.isObject({ key : "value" });
    expect(true_result).to.equal(true);

    const false_result = util.isObject(4);
    expect(false_result).to.equal(false);
  });
});

describe('isNull', () => {

  it('should return true for null and false for other', () => {
    const true_result = util.isNull(null);
    expect(true_result).to.equal(true);

    const false_result = util.isNull(4);
    expect(false_result).to.equal(false);
  });
});

describe('isNull', () => {

  it('should return true for undefined and false for other', () => {
    const true_result = util.isUndefined(undefined);
    expect(true_result).to.equal(true);

    const false_result = util.isUndefined(4);
    expect(false_result).to.equal(false);
  });
});

describe('isUndefined', () => {

  it('should return true for undefined and false for other', () => {
    const true_result = util.isUndefined(undefined);
    expect(true_result).to.equal(true);

    const false_result = util.isUndefined(4);
    expect(false_result).to.equal(false);
  });
});

describe('isBoolean', () => {

  it('should return true for a boolean and false for other', () => {
    const true_result = util.isBoolean(false);
    expect(true_result).to.equal(true);

    const false_result = util.isBoolean(4);
    expect(false_result).to.equal(false);
  });
});

describe('isRegExp', () => {

  it('should return true for a regular expression and false for other', () => {
    const true_result = util.isRegExp(/^[1-9]\d{0,2}$/g);
    expect(true_result).to.equal(true);

    const false_result = util.isRegExp(4);
    expect(false_result).to.equal(false);
  });
});

describe('isError', () => {

  it('should return true for an error and false for other', () => {
    const true_result = util.isError(Error("My Error"));
    expect(true_result).to.equal(true);

    const false_result = util.isError(4);
    expect(false_result).to.equal(false);
  });
});

describe('isDate', () => {

  it('should return true for a date and false for other', () => {
    const true_result = util.isDate(new Date());
    expect(true_result).to.equal(true);

    const false_result = util.isDate(4);
    expect(false_result).to.equal(false);
  });
});

describe('isSymbol', () => {

  it('should return true for a symbol and false for other', () => {
    const true_result = util.isSymbol(Symbol("key"));
    expect(true_result).to.equal(true);

    const false_result = util.isSymbol(4);
    expect(false_result).to.equal(false);
  });
});
