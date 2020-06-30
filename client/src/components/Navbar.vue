<template>
  <b-navbar toggleable="lg" type="dark" variant="primary">
    <b-navbar-brand to="/">Voltron</b-navbar-brand>
    <b-navbar-toggle target="nav-collapse"></b-navbar-toggle>
    <b-collapse id="nav-collapse" is-nav>
      <b-navbar-nav>
        <b-nav-item-dropdown v-if="isSignedIn" text="Classes" right>
          <b-dropdown-header>Instructor</b-dropdown-header>
          <b-dropdown-item v-on:click="setInstructor(1)">FIXME1</b-dropdown-item>
          <b-dropdown-item v-on:click="setInstructor(2)">FIXME2</b-dropdown-item>
          <b-dropdown-item v-on:click="setInstructor(3)">FIXME3</b-dropdown-item>
          <b-dropdown-divider />
          <b-dropdown-header>Student</b-dropdown-header>
          <b-dropdown-item v-on:click="setStudent(4)">FIXME4</b-dropdown-item>
          <b-dropdown-item v-on:click="setStudent(5)">FIXME5</b-dropdown-item>
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

  setInstructor(x: number) {
    console.log("class-instructor", x);
  }

  setStudent(x: number) {
    console.log("class-student", x);
  }
}
</script>
