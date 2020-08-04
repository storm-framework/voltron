import Vue from "vue";
import App from "./App.vue";
import BootstrapVue from "bootstrap-vue";

// import "./registerServiceWorker";
import router from "./router";
import store from "./store";
// import 'bootstrap/dist/css/bootstrap.css';
import 'bootswatch/dist/spacelab/bootstrap.css';
import 'bootstrap-vue/dist/bootstrap-vue.css';

Vue.use(BootstrapVue);
Vue.config.productionTip = false;


new Vue({
  router,
  store,
  render: h => h(App)
}).$mount("#app");
