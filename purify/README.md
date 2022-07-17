# Usage

This program purifies the content in an HTML file (e.g. from `page_dl`) and returns only the cleaned text content.

```sh
npm install
chmod +x read
chmod +x purify
```

To purify an HTML file

```sh
./read test.html | ./purify 
```

You can delete the json file (e.g. "test.html.json") afterwards.
