// Import testing libs
import {expect} from 'chai';

// Import package under test
import {Resource, ResourceSet} from '../../src/ts/resource';


describe('ResourceSet', () => {
  const inTest = new ResourceSet();
  const myResource: Resource = {
    basename: 'test.file',
    contents: 'test.contents',
  };

  const myOtherResource: Resource = {
    basename: 'test.file',
    contents: 'test.other contents',
  };

  const myNewResource: Resource = {
    basename: 'another.file',
    contents: 'other.other contents',
  };

  const anotherResource: Resource = {
    basename: 'new filename',
    contents: 'test.contents',
  };

  describe('#addWeak()', () => {
    it('should add a resource to the set', () => {
      expect(inTest).to.have.length(0);
      const result = inTest.addWeak(myResource);
      expect(result).to.be.true;
      expect(inTest).to.have.length(1);
      expect(inTest.raw).to.deep.equal([myResource]);
    });

    it('should not add a resource with the same name as an existing', () => {
      const result = inTest.addWeak(myOtherResource);
      expect(result).to.be.false;
      expect(inTest).to.have.length(1);
      expect(inTest.raw).to.deep.equal([myResource]);
    });
  });

  describe('#addUnique()', () => {
    it('should add a unique resource without an exception', () => {
      inTest.addUnique(myNewResource);
      expect(inTest).to.have.length(2);
      expect(inTest.raw).to.deep.equal([myResource, myNewResource]);
    });

    it('should throw an exception when a non unique resource is added', () => {
      expect(() => {
        inTest.addUnique(myResource);
      }).to.throw(Error);
      expect(inTest).to.have.length(2);
    });
  });

  describe('#addOverwrite()', () => {
    it('should overwrite an existing file with the same name', () => {
      const result = inTest.addOverwrite(myOtherResource);
      expect(result).to.be.false;
      expect(inTest).to.have.length(2);
      expect(inTest.raw).to.deep.equal([myOtherResource, myNewResource]);
    });

    it('should not overwrite anything when a unique file is added', () => {
      const result = inTest.addOverwrite(anotherResource);
      expect(result).to.be.true;
      expect(inTest).to.have.length(3);
      expect(inTest.raw).to.deep.
          equal([myOtherResource, myNewResource, anotherResource]);
    });
  });

  describe('#addOverwriteList()', () => {
    const r1: Resource = {
      basename: 'r1',
      contents: 'r1 contents',
    };
    const r2: Resource = {
      basename: myOtherResource.basename,
      contents: myOtherResource.contents + ' modified',
    };

    const mergeSet = new ResourceSet();
    mergeSet.addUnique(r1);
    mergeSet.addUnique(r2);

    it('should merge the two sets and overwrite the existing', () => {
      inTest.addListOverwrite(mergeSet);
      expect(inTest).to.have.length(4);
      expect(inTest.raw).to.deep.equal([r2, myNewResource, anotherResource, r1]);
    });
  });
});
