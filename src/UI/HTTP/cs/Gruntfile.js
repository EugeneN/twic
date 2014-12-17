module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

      srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"]

    , dotPsci: ["<%=srcFiles%>"]

    , concat: {
          options: {separator: ""}
        , basic: {
              src: [
                  "bower_components/jquery/dist/jquery.js"
                , "bower_components/rxjs/dist/rx.all.js"
                , "bower_components/rxjs-jquery/rx.jquery.js"
                , "bower_components/react/react.js"
                , "../../../../dist/cs/App.js"
              ],
              dest:  "../../../../dist/cs/Main.js"
          }
      }

    , psc: {
          options: {
              main: "Main"
            , modules: ["Main"]
          }
        , all: {
              src: ["<%=srcFiles%>"]
            , dest: "../../../../dist/cs/App.js"
          }
      }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks('grunt-contrib-concat');
  
  grunt.registerTask("default", ["psc:all", "concat:basic"]);
};
