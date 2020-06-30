<template>
  <b-navbar toggleable="lg" type="dark" variant="primary">
    <b-navbar-brand to="/">Voltron</b-navbar-brand>
    <b-navbar-toggle target="nav-collapse"></b-navbar-toggle>
    <b-collapse id="nav-collapse" is-nav>
      <b-navbar-nav>
        <b-nav-item-dropdown v-if="isSignedIn" text="Classes" right>
          <b-dropdown-item href="#">EN</b-dropdown-item>
          <b-dropdown-item href="#">ES</b-dropdown-item>
          <b-dropdown-item href="#">RU</b-dropdown-item>
          <b-dropdown-item href="#">FA</b-dropdown-item>
        </b-nav-item-dropdown>

        <b-nav-item to="/about">About</b-nav-item>

        <b-nav-item to="/contact">Contact</b-nav-item>

        <b-nav-item v-if="isSignedIn" to="/signout">Logout</b-nav-item>
      </b-navbar-nav>
    </b-collapse>
  </b-navbar>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";

@Component
export default class Navbar extends Vue {
  // Nothing exciting here?
  name = "Navigation";

  get isSignedIn() {
    return this.$store.getters.isSignedIn;
  }
}
</script>
