module.exports = {
  testEnvironment: 'jsdom',
  "preset": "ts-jest",
  verbose: true,
  forceExit: true,
  testNamePattern: "",
  watchAll: false,
  ci: true,
  rootDir: '.',
  transform: {
    '\\.js$': ['babel-jest', { configFile: './_babel.config.json' }]
  },
  testMatch: ["**/*.test.fs.js"],
  testPathIgnorePatterns: [".fable"]
};
