<template>
  <b-navbar toggleable="lg" type="dark" variant="primary">
    <b-navbar-brand
      :to="{ name: 'Home', params: { classId: $store.getters.currentClassId }}"
      >Voltron</b-navbar-brand
    >
    <b-navbar-toggle target="nav-collapse"></b-navbar-toggle>
    <b-collapse id="nav-collapse" is-nav>
      <b-navbar-nav>
        <b-nav-item-dropdown v-if="isSignedIn" text="Classes" right>
          <template v-if="isInstructor">
            <b-dropdown-header>Instructor</b-dropdown-header>
            <b-dropdown-item
              v-for="item in instructorClasses"
              :key="item.index"
              v-on:click="setClass(item.index)"
            >
              {{ item.name }}
            </b-dropdown-item>
          </template>
          <b-dropdown-divider v-if="isInstructor && isStudent" />
          <template v-if="isStudent">
            <b-dropdown-header>Student</b-dropdown-header>
            <b-dropdown-item
              v-for="item in studentClasses"
              :key="item.index"
              v-on:click="setClass(item.index)"
            >
              {{ item.name }}
            </b-dropdown-item>
          </template>
        </b-nav-item-dropdown>
        <b-nav-item to="/contact">Contact</b-nav-item>
        <b-nav-item v-if="isSignedIn" v-on:click="signOut()">Logout</b-nav-item>
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

  get instructorClasses() {
    const classes = this.$store.getters.instructorClasses;
    console.log("instructor-classes", classes);
    return classes;
  }

  get isInstructor() {
    return this.instructorClasses.length > 0;
  }

  get studentClasses() {
    const classes = this.$store.getters.studentClasses;
    console.log("student-classes", classes);
    return classes;
  }

  get isStudent() {
    return this.studentClasses.length > 0;
  }

  setClass(classId: string) {
    console.log("set-class", classId);
    this.$store.commit("setCurrentClass", classId);
    this.$router.push({ name: "Home", params: { classId } });
  }

  signOut() {
    this.$store.dispatch("signOut");
    this.$router.push({ name: "Login" });
  }

  // mounted() {
  //   console.log("syncUser");
  //   this.$store.dispatch("syncSessionUserData").catch(error => {
  //     // Don't panic if the user is not yet authenticated
  //     if (error?.response?.status !== 401) {
  //       throw error;
  //     }
  //   });
  // }
}
</script>
