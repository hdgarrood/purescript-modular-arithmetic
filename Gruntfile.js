module.exports = function(grunt) {
  "use strict";

  var libFiles = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs",
  ]
  var testsFiles = ["tests/**/*.purs"].concat(libFiles)

  grunt.initConfig({
    libFiles: libFiles,

    pscMake: {
      all: { src: libFiles }
    },
    dotPsci: {
      src: libFiles
    },
    pscDocs: {
      all: {
        src: "src/Data/ModularArithmetic.purs",
        dest: "docs/Data.ModularArithmetic.md"
      },
    },
  });

  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("make",    ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["make"]);
};
