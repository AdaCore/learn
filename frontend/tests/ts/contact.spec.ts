// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';
import chaiAsPromised from 'chai-as-promised';

chai.use(chaiDom);
chai.use(chaiAsPromised);

import fetchMock from 'fetch-mock';

// Import package under test
import {ContactForm, contactFactory} from '../../src/ts/contact';
import * as Strings from '../../src/ts/strings';

describe('contactFactory()', () => {
  let htmlList: HTMLCollectionOf<Element>;
  let inTestList: Array<ContactForm>;
  let contactForm: HTMLDivElement;
  let form: HTMLFormElement;

  beforeEach(() => {
    contactForm = document.createElement('div');
    contactForm.classList.add('contact-form');
    contactForm.setAttribute('example_server', 'http://example.com');

    document.body.appendChild(contactForm);

    form = document.createElement('form');
    contactForm.appendChild(form);
  });

  afterEach(() => {
    document.body.innerHTML = '';
  });

  it('should be a list with 1 elements', () => {
    htmlList = document.getElementsByClassName('contact-form');
    inTestList = contactFactory(htmlList);

    expect(inTestList).to.have.lengthOf(1);
    expect(inTestList[0]).to.be.an.instanceof(ContactForm);
  });

  it('should create div with error when no server addr is found', () => {
    contactForm.removeAttribute('example_server');
    htmlList = document.getElementsByClassName('contact-form');
    inTestList = contactFactory(htmlList);

    expect(inTestList).to.have.length(0);
    expect(contactForm).to.have.descendants('div').with.length(1);
    const errorDiv = contactForm.querySelector('div');
    expect(errorDiv).to.have.html(
        '<p>An error has occured processing this form. ' +
        Strings.INTERNAL_ERROR_MESSAGE + '</p>');
  });

  it('should create div with error when no form tag found', () => {
    contactForm.removeChild(form);
    htmlList = document.getElementsByClassName('contact-form');
    inTestList = contactFactory(htmlList);

    expect(inTestList).to.have.length(0);
    expect(contactForm).to.have.descendants('div').with.length(1);
    const errorDiv = contactForm.querySelector('div');
    expect(errorDiv).to.have.html(
        '<p>An error has occured processing this form. ' +
        Strings.INTERNAL_ERROR_MESSAGE + '</p>');
  });
});

describe('ContactForm', () => {
  const baseURL = 'http://example.com';
  let htmlList: HTMLCollectionOf<Element>;
  let inTestList: Array<ContactForm>;

  const contactForm = document.createElement('div');
  contactForm.classList.add('contact-form');
  contactForm.setAttribute('example_server', baseURL);

  const form = document.createElement('form');
  contactForm.appendChild(form);

  before(() => {
    document.body.appendChild(contactForm);
    htmlList = document.getElementsByClassName('contact-form');
    inTestList = contactFactory(htmlList);
  });

  after(() => {
    document.body.innerHTML = '';
  });

  it('should have fields and a gdpr textbox', () => {
    expect(inTestList).to.have.length(1);
    expect(form).to.have.descendant('#name-field');
    expect(form).to.have.descendant('#email-field');
    expect(form).to.have.descendant('#message-field');
    expect(form).to.have.descendant('div.form-checkbox');
  });

  it('should run validation func on name field on focusout', () => {
    expect(form).not.to.have.descendants('span.form-error');
    expect(form).to.have.descendant('#name-field');
    const nameField = form.querySelector('#name-field');
    expect(nameField).to.have.tagName('input');
    (nameField as HTMLInputElement).focus();
    (nameField as HTMLInputElement).blur();
    expect(form).to.have.descendants('span.form-error').and.have.length(1);
    const err = form.querySelector('span.form-error');
    expect(err).to.contain.text('Name field cannot be empty.');
  });

  it('should remove error when correct value supplied', () => {
    expect(form).to.have.descendants('span.form-error').and.have.length(1);
    const err = form.querySelector('span.form-error');
    expect(err).to.contain.text('Name field cannot be empty.');
    const nameField = form.querySelector('#name-field') as HTMLInputElement;
    nameField.focus();
    nameField.value = 'Test Name';
    nameField.blur();
    expect(form).not.to.have.descendants('span.form-error');
  });

  it('should run validation func on email field on focusout', () => {
    expect(form).not.to.have.descendants('span.form-error');
    expect(form).to.have.descendant('#email-field');
    const emailField = form.querySelector('#email-field');
    expect(emailField).to.have.tagName('input');
    (emailField as HTMLInputElement).focus();
    (emailField as HTMLInputElement).value = 'invalid.email@wrong';
    (emailField as HTMLInputElement).blur();
    expect(form).to.have.descendants('span.form-error').and.have.length(1);
    const err = form.querySelector('span.form-error');
    expect(err).to.contain.text('Email address is invalid.');
  });

  it('should remove error when correct value supplied', () => {
    expect(form).to.have.descendants('span.form-error').and.have.length(1);
    const err = form.querySelector('span.form-error');
    expect(err).to.contain.text('Email address is invalid.');
    const emailField = form.querySelector('#email-field') as HTMLInputElement;
    emailField.focus();
    emailField.value = 'test@test.com';
    emailField.blur();
    expect(form).not.to.have.descendants('span.form-error');
  });

  it('should run validation func on message field on focusout', () => {
    expect(form).not.to.have.descendants('span.form-error');
    expect(form).to.have.descendant('#message-field');
    const msgField = form.querySelector('#message-field');
    expect(msgField).to.have.tagName('textarea');
    (msgField as HTMLInputElement).focus();
    (msgField as HTMLInputElement).blur();
    expect(form).to.have.descendants('span.form-error').and.have.length(1);
    const err = form.querySelector('span.form-error');
    expect(err).to.contain.text('Message field cannot be empty.');
  });

  it('should remove error when correct value supplied', () => {
    expect(form).to.have.descendants('span.form-error').and.have.length(1);
    const err = form.querySelector('span.form-error');
    expect(err).to.contain.text('Message field cannot be empty.');
    const msgField = form.querySelector('#message-field') as HTMLInputElement;
    msgField.focus();
    msgField.value = 'Test message';
    msgField.blur();
    expect(form).not.to.have.descendants('span.form-error');
  });

  it('should protect the submit button from erroneous submission', () => {
    expect(form).to.have.descendants('button.form-submit').and.have.length(1);
    const submit = form.querySelector('button.form-submit');
    expect(submit).to.have.class('disabled');

    const nameField = form.querySelector('#name-field') as HTMLInputElement;
    expect(nameField.value).to.equal('Test Name');
    const emailField = form.querySelector('#email-field') as HTMLInputElement;
    expect(emailField.value).to.equal('test@test.com');
    const msgField = form.querySelector('#message-field') as HTMLInputElement;
    expect(msgField.value).to.equal('Test message');

    expect(form).to.have.descendants('input.checkbox').and.have.length(1);
    const gdpr = form.querySelector('input.checkbox') as HTMLInputElement;

    expect(gdpr.checked).to.be.false;
    gdpr.click();
    expect(gdpr.checked).to.be.true;
    expect(submit).not.to.have.class('disabled');
  });

  describe('form submission tests', () => {
    before(() => {
      fetchMock.post(baseURL + '/contact_form/', {
        body: {
          'success': false,
        },
      }, {
        repeat: 1,
      });
      fetchMock.post(baseURL + '/contact_form/', {
        body: {
          'success': true,
        },
      }, {
        repeat: 1,
        overwriteRoutes: false,
      });
    });

    after(() => {
      fetchMock.reset();
    });

    it('should send data to server and handle error', async () => {
      const btn = form.querySelector('button.form-submit') as HTMLButtonElement;
      btn.click();

      await fetchMock.flush(true);

      const formSubmit = fetchMock.calls(baseURL + '/contact_form/');
      expect(formSubmit[0][1]['body']).to.equal(JSON.stringify({
        'Name': 'Test Name',
        'Email': 'test@test.com',
        'Message': 'Test message',
        'GDPRConsent': true,
      }));

      expect(contactForm).to.have.descendants('div.form-fail').
          and.have.length(1);
    });

    it('should send data to server and reset form', async () => {
      const btn = form.querySelector('button.form-submit') as HTMLButtonElement;
      btn.click();

      await fetchMock.flush(true);

      const formSubmit = fetchMock.calls(baseURL + '/contact_form/');
      expect(formSubmit[1][1]['body']).to.equal(JSON.stringify({
        'Name': 'Test Name',
        'Email': 'test@test.com',
        'Message': 'Test message',
        'GDPRConsent': true,
      }));

      expect(btn).to.have.class('disabled');
      expect(contactForm).to.have.descendants('div.form-success').
          and.have.length(1);

      const nameField = form.querySelector('#name-field') as HTMLInputElement;
      expect(nameField.value).to.equal('');
      const emailField = form.querySelector('#email-field') as HTMLInputElement;
      expect(emailField.value).to.equal('');
      const msgField = form.querySelector('#message-field') as HTMLInputElement;
      expect(msgField.value).to.equal('');
      const gdpr = form.querySelector('input.checkbox') as HTMLInputElement;
      expect(gdpr.checked).to.be.false;
    });
  });
});
