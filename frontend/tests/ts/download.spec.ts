import chai, {expect} from 'chai';
import chaiAsPromised from 'chai-as-promised';
chai.use(chaiAsPromised);

import {
  getLanguages,
  parseSwitches,
  findMains,
  getMain,
  getGprContents,
} from '../../src/ts/download';
import {ResourceList} from '../../src/ts/resource';

describe('Download', () => {
  describe('#getLanguages()', () => {
    it('should find no languages', () => {
      const files: ResourceList = [];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ();');
    });

    it('should find Ada spec', () => {
      const files: ResourceList = [{basename: 'test.ads', contents: ''}];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ("Ada");');
    });

    it('should find Ada body', () => {
      const files: ResourceList = [{basename: 'test.adb', contents: ''}];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ("Ada");');
    });

    it('should find c header', () => {
      const files: ResourceList = [{basename: 'test.h', contents: ''}];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ("c");');
    });

    it('should find c body', () => {
      const files: ResourceList = [{basename: 'test.c', contents: ''}];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ("c");');
    });

    it('should find c++ header', () => {
      const files: ResourceList = [{basename: 'test.hh', contents: ''}];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ("c++");');
    });

    it('should find c++ body', () => {
      const files: ResourceList = [{basename: 'test.cpp', contents: ''}];
      const languages = getLanguages(files);
      expect(languages).to.equals('for Languages use ("c++");');
    });

    it('should find Ada, c, and c++', () => {
      const files: ResourceList = [
        {basename: 'test.adb', contents: ''},
        {basename: 'test.h', contents: ''},
        {basename: 'test.cpp', contents: ''},
      ];
      const languages = getLanguages(files);
      const languagesRegex =
        /for Languages use \((?=.*"Ada",?)(?=.*"c",?)(?=.*"c\+\+",?).*\);/;
      expect(languagesRegex.test(languages)).to.equals(true);
    });
  });

  describe('#parseSwitches()', () => {
    it('should find no switches', () => {
      const parsedSwitches = parseSwitches('{}');
      const expectedBuilder = 'for Switches ("Ada") use ();';
      const expectedCompiler = 'for Switches ("Ada") use ();';
      expect(parsedSwitches['--BUILDER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedBuilder
      );
      expect(parsedSwitches['--COMPILER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedCompiler
      );
    });

    it('should find builder switches', () => {
      const parsedSwitches = parseSwitches('{"Builder": ["test1", "test2"]}');
      const expectedBuilder = 'for Switches ("Ada") use ("test1", "test2");';
      const expectedCompiler = 'for Switches ("Ada") use ();';
      expect(parsedSwitches['--BUILDER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedBuilder
      );
      expect(parsedSwitches['--COMPILER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedCompiler
      );
    });

    it('should find compiler switches', () => {
      const parsedSwitches = parseSwitches('{"Compiler": ["test3", "test4"]}');
      const expectedBuilder = 'for Switches ("Ada") use ();';
      const expectedCompiler = 'for Switches ("Ada") use ("test3", "test4");';
      expect(parsedSwitches['--BUILDER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedBuilder
      );
      expect(parsedSwitches['--COMPILER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedCompiler
      );
    });

    it('should find both switches', () => {
      const parsedSwitches = parseSwitches(
        '{"Compiler": ["test3", "test4"], "Builder": ["test1", "test2"]}'
      );
      const expectedBuilder = 'for Switches ("Ada") use ("test1", "test2");';
      const expectedCompiler = 'for Switches ("Ada") use ("test3", "test4");';
      expect(parsedSwitches['--BUILDER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedBuilder
      );
      expect(parsedSwitches['--COMPILER_SWITCHES_PLACEHOLDER--']).to.equal(
        expectedCompiler
      );
    });
  });

  describe('#findMains()', () => {
    it('should find no mains (no files)', () => {
      const files: ResourceList = [];
      const mains = findMains(files);
      expect(mains).to.be.empty;
    });

    it('should assume a single file is the main', () => {
      const files: ResourceList = [{basename: 'test.xyz', contents: ''}];
      const mains = findMains(files);
      expect(mains).to.have.lengthOf(1);
      expect(mains).to.include('test.xyz');
    });

    it('should find no mains (no adb)', () => {
      const files: ResourceList = [
        {basename: 'test.ads', contents: ''},
        {basename: 'test.ads', contents: ''},
      ];
      const mains = findMains(files);
      expect(mains).to.be.empty;
    });

    it('should find no mains (no unpaired adb)', () => {
      const files: ResourceList = [
        {basename: 'test.ads', contents: ''},
        {basename: 'test.adb', contents: ''},
      ];
      const mains = findMains(files);
      expect(mains).to.be.empty;
    });

    it('should find a main', () => {
      const files: ResourceList = [
        {basename: 'test.ads', contents: ''},
        {basename: 'other.adb', contents: ''},
        {basename: 'test.adb', contents: ''},
      ];
      const mains = findMains(files);
      expect(mains).to.have.lengthOf(1);
      expect(mains).to.include('other.adb');
    });

    it('should find two mains', () => {
      const files: ResourceList = [
        {basename: 'other.adb', contents: ''},
        {basename: 'test.adb', contents: ''},
      ];
      const mains = findMains(files);
      expect(mains).to.have.lengthOf(2);
      expect(mains).to.include.members(['other.adb', 'test.adb']);
    });

    it('should find no mains (c file)', () => {
      const files: ResourceList = [
        {basename: 'other.c', contents: ''},
        {basename: 'test.ads', contents: ''},
        {basename: 'test.adb', contents: ''},
      ];
      const mains = findMains(files);
      expect(mains).to.be.empty;
    });

    it('should find a main (c file)', () => {
      const cFile = `#include <stdio.h>\nint main() {\nprintf("Hello, World!");\nreturn 0;\n}`;
      const files: ResourceList = [
        {basename: 'other.c', contents: cFile},
        {basename: 'test.ads', contents: ''},
        {basename: 'test.adb', contents: ''},
      ];
      const mains = findMains(files);
      expect(mains).to.have.lengthOf(1);
      expect(mains).to.include.members(['other.c']);
    });
  });

  describe('#getMain()', () => {
    it('should default to using the provided main', () => {
      const files: ResourceList = [{basename: 'other.adb', contents: ''}];
      const main = getMain(files, 'main.adb');
      const expectedResult = 'main';
      expect(main).to.equal(expectedResult);
    });

    it('should find a main if one is not provided', () => {
      const files: ResourceList = [{basename: 'other.adb', contents: ''}];
      const main = getMain(files, '');
      const expectedResult = 'other';
      expect(main).to.equal(expectedResult);
    });

    it('should error if more than one main is found', () => {
      const origConsoleError = console.error;
      let errorText = '';
      console.error = (msg) => {
        errorText = `${errorText}\n${msg}`;
      };
      const files: ResourceList = [
        {basename: 'other1.adb', contents: ''},
        {basename: 'other2.adb', contents: ''},
      ];
      getMain(files, '');
      expect(errorText).to.not.be.empty;
      console.error = origConsoleError;
    });
  });

  describe('#getGprContents()', () => {
    it('should replace all placeholders', () => {
      const files: ResourceList = [{basename: 'main.adb', contents: ''}];
      const gpr = getGprContents(files, '{}', 'main.adb');
      expect(gpr).to.not.contain('--MAIN_PLACEHOLDER--');
      expect(gpr).to.not.contain('--LANGUAGE_PLACEHOLDER--');
      expect(gpr).to.not.contain('--COMPILER_SWITCHES_PLACEHOLDER--');
      expect(gpr).to.not.contain('--BUILDER_SWITCHES_PLACEHOLDER--');
      expect(gpr).to.not.contain('--');
    });
  });
});
