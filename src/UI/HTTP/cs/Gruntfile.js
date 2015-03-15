module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

      srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"]
    , justSrcFiles: ["src/**/*.purs"]

    , dotPsci: ["<%=srcFiles%>"]

    , concat: {
          options: {separator: ""}
        , basic: {
              src: [
                  "bower_components/jquery/dist/jquery.js"
                , "bower_components/rxjs/dist/rx.all.js"
                , "bower_components/rxjs-jquery/rx.jquery.js"
                , "bower_components/react/react.js"
                , "./res/App.js"
              ],
              dest:  "./res/Main.js"
          }
          , cjs: {
              src: [
                    "bower_components/jquery/dist/jquery.js"
                  , "bower_components/rxjs/dist/rx.all.js"
                  , "bower_components/rxjs-jquery/rx.jquery.js"
                  , "bower_components/react/react.js"
                  , "./res/cjs/**.js"
              ],
              dest:  "./res/cjs/CJSMain.js"
          }
      }

    , psc: {
          options: {
              main: "Main"
            , modules: ["Main"]
          }
        , all: {
              src: ["<%=srcFiles%>"]
            , dest: "./res/App.js"
          }
        , just: {
              src: ["<%=justSrcFiles%>"]
            , dest: "./res/justApp.js"
          }
      }

    , pscMake: {
        options: {
            main: "Main"
          , modules: ["Main"]
        }
      , all: {
          src: ["<%=srcFiles%>"]
        , dest: "./res/cjs/"
      }
      , just: {
          src: ["<%=justSrcFiles%>"]
          , dest: "./res/cjsJustApp.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks('grunt-contrib-concat');
  
  grunt.registerTask("default", ["psc:all", "concat:basic"]);
};
