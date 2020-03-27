import $ from 'jquery';

import {Form, FormData} from './components';

export class ContactForm {
  private container: JQuery;
  private server: string;
  private form: Form;

  /**
   * Constructs the ContactForm
   * @param {JQuery} container - the container for the contact form
   * @param {string} server - the server address:port
   */
  constructor(container: JQuery, server: string) {
    this.container = container;
    this.server = server;

    this.form = new Form('Contact Us', 'Submit', (data: Array<FormData>) => {
      // TODO: figure out form submission
      console.log('Submitting form with data:' + data);
    });
    this.form.addInput('Name', 'name', 'Name', (data: string): boolean => {
      return data.length != 0;
    });
    this.form.addInput('Email', 'email', 'Email', (data: string): boolean => {
      const emailRegex = /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/;
      if (emailRegex.test(data)) {
        return true;
      }
      return false;
    });
    this.form.addTextArea('Message', 'message', 'Message', 5, (data: string): boolean => {
      return data.length != 0;
    });
  }

  /**
  * Render the widget by putting it into this.container
  */
  public render(): JQuery {
    return this.container.append(this.form.render());
  }
}