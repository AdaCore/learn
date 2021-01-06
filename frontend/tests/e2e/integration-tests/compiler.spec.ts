import * as puppeteer from 'puppeteer';
import {expect} from 'chai';

describe('E2E', function () {
  let browser: puppeteer.Browser;
  let page: puppeteer.Page;

  before(async () => {
    browser = await puppeteer.launch({
      args: [
        // Required for Docker version of Puppeteer
        '--no-sandbox',
        '--disable-setuid-sandbox',
        // This will write shared memory files into /tmp instead of /dev/shm,
        // because Docker’s default for /dev/shm is 64MB
        '--disable-dev-shm-usage'
      ]
    });

    const browserVersion = await browser.version();
    console.log(`Started ${browserVersion}`);
  });

  after(() => {
    browser.close();
  })

  beforeEach(async() => {
    page = await browser.newPage();
  });

  afterEach(async() => {
    await page.close();
  });

  describe('App', () => {
    it('runs a widget', async () => {
      const response = await page.goto('http://web/');
      expect(response.ok()).to.be.true;



      const widgets = await page.$$('widget');

      console.log('Found ' + widgets.length + ' widgets.');

      for (const w of widgets) {
        const id = await page.evaluate(el => el.getAttribute('id'), w);
        const button_group = await page.$('#' + id + '.button-group');
        const buttons = await button_group.$$('button');
        for (const b of buttons) {
          await b.click();
        }
      }
    });
  });
});
