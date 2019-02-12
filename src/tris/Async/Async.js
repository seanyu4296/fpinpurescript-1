var fs = require('fs');

exports.readFileImpl = function(path, onSuccess, onFailure) {
  return function() {
    fs.readFile(path, { encoding: 'utf-8' }, function(err, data) {
      if (err) {
        onFailure(err.code)();
      } else {
        onSuccess(data)();
      }
    });
  };
};

exports.writeFileImpl = function(path, data, onSuccess, onFailure) {
  return function() {
    fs.writeFile(path, data, { encoding: 'utf-8' }, function(error) {
      if (error) {
        onFailure(error.code)();
      } else {
        onSuccess();
      }
    });
  };
};
