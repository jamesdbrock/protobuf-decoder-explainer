import {main} from '../output/Main/index.js';

main();

if (module.hot) {
  module.hot.accept();
}

console.log('app starting');
