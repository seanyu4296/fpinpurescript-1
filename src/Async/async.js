exports.fromCb = function(f) {
  return function() {
    return new Promise(function(resolve) {
      resolve(f());
    });
  };
};

exports.runAsync = function(async) {
  return function() {
    async();
  };
};
