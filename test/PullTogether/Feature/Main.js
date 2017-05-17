var fs = require("fs");

exports.createReadStream = function(from) {
    return function(cb, eb) {
        try {
            return cb(fs.createReadStream(from));
        } catch (e) {
            return eb(e);
        }
    };
};

exports.createWriteStream = function(to) {
    return function(cb, eb) {
        try {
            return cb(fs.createWriteStream(to));
        } catch (e) {
            return eb(e);
        }
    };
};

exports.stack = function(e) {
    return e.stack;
};
