const React = require('react')
const ReactDOM = require('react-dom')
const Main = require('./output/Main/index.js')

ReactDOM.render(
  React.createElement(Main.main),
  document.getElementById('container')
)