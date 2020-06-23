<template>
  <div id="app">
    <navbar />

    <b-container>
      <router-view />
    </b-container>

    <!--  <div>
  <b-jumbotron>
    <template v-slot:header>BootstrapVue</template>

    <template v-slot:lead>
      This is a simple hero unit, a simple jumbotron-style component for calling extra attention to
      featured content or information.
    </template>

    <hr class="my-4">

    <p>
      It uses utility classes for typography and spacing to space content out within the larger
      container.
    </p>

    <b-button variant="primary" href="#">Do Something</b-button>
    <b-button variant="success" href="#">Do Something Else</b-button>
  </b-jumbotron> 
</div>-->
  </div>
</template>

<style>
#app {
  /* font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50; */
  width: 100%;
  height: 100%;
}

#nav {
  padding: 30px;
}

#nav a {
  font-weight: bold;
  color: #2c3e50;
}

#nav a.router-link-exact-active {
  color: #42b983;
}

.firepad {
  -webkit-box-sizing: border-box;
  -moz-box-sizing: border-box;
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  position: relative;
  text-align: left;
  line-height: normal;
}

#editor {
  /** Setting height is also important, otherwise editor wont showup**/
  height: 300px;
}
</style>

<style lang="scss">
@import "~bootstrap/scss/bootstrap";
@import "~bootstrap-vue/src/index.scss";
@import "~bootswatch/dist/flatly/bootstrap.css";

html,
body {
  height: 100%;
}

// body {
//   background-color: #f5f5f5;
//   font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
//     "Helvetica Neue", Arial, "Noto Sans", sans-serif, "Apple Color Emoji",
//     "Segoe UI Emoji", "Segoe UI Symbol" !important;
// }
</style>

<script lang="ts">
import { Vue, Component } from "vue-property-decorator";
// import { BootstrapVue } from "bootstrap-vue";

import Navbar from "@/components/Navbar.vue";
import router from "@/router";
import store from "@/store";

@Component({
  store,
  router,
  components: { Navbar }
})
export default class App extends Vue {}
</script>
