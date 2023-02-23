var express = require("express")
var app = express()

app.use(express.static('app'))
// app.use('/js', express.static(__dirname__ + '/'))
// app.use(express.static())

var server = app.listen(8080, () => {
  var port = server.address().port
  console.log(`Server started at http://localhost:${port}`);
})