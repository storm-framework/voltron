<template>
  <b-container>
    <div v-for="studentClass in studentClasses" v-bind:key="studentClass.index">
      <div v-show="isCurrent(studentClass.index)">
        <div class="page-header">
          <b-row>
            <b-col lg="12" md="8" sm="4">
              <h2 class="d-inline">{{ studentClass.name }}</h2>
              <b-button variant="info" size="lg" class="float-right">
                Student: {{ studentName }}
              </b-button>
            </b-col>
          </b-row>
        </div>
        <br />
        <b-row>
          <b-col lg="12" md="8" sm="4">
            <div class="card border-primary mb-12">
              <div class="card-header">
                <!-- <b-dropdown id="dropdown-1" class="m-md-2"> -->
                <b-dropdown split split-variant="outline-primary" variant="primary" class="m-md-2">
                  <template #button-content>Group {{ studentClass.data.grpBuffer.id }}</template>
                  <b-dropdown-item
                    v-for="item in studentClass.data.allGroups"
                    :key="item.index"
                    v-on:click="setGroup(item)"
                  >
                    {{ item }}
                  </b-dropdown-item>
                  <!-- <b-dropdown-item>First Action</b-dropdown-item>
                  <b-dropdown-item>Second Action</b-dropdown-item>
                  <b-dropdown-item>Third Action</b-dropdown-item> -->
                </b-dropdown>
              </div>
              <div class="card-body">
                <div v-bind:id="studentClass.data.grpBuffer.div"></div>
              </div>
            </div>
            <!-- <div class="move-group">
              <b-dropdown id="dropdown-1" text="Dropdown Button" class="m-md-2">
                <b-dropdown-item>First Action</b-dropdown-item>
                <b-dropdown-item>Second Action</b-dropdown-item>
                <b-dropdown-item>Third Action</b-dropdown-item>
              </b-dropdown>
            </div> -->
          </b-col>
        </b-row>
      </div>
    </div>
  </b-container>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import BufferService from "@/services/buffer";
import { Student, ClassView, EnrollStudent, SetGroup } from "../types";
import ApiService from "@/services/api";

@Component
export default class StudentVue extends Vue {
  name = "Student";

  get studentName() {
    return this.$store.getters.currentUser.firstName;
  }

  get className() {
    return this.$store.getters.currentClass.class;
  }

  get classLanguage() {
    return this.$store.getters.currentClass.language;
  }

  get studentClasses(): Array<ClassView<Student>> {
    return this.$store.getters.studentClasses;
  }

  setGroup(group: number) {
    console.log("setGroup: ", group);
    // this.selectedLang = language;
    const klass = this.className;
    const email = this.$store.getters.currentUser.email;
    console.log("setGroup: ", klass, email, group);
    const info: SetGroup = { student: email, class: klass, group };
    ApiService.setGroup(info)
      .then(() => this.$store.dispatch("syncSessionUserData"))
      .catch(error =>
        this.showError("Unexpected error: " + error.response?.status)
      );
  }

  isCurrent(index: number): boolean {
    return this.$store.getters.currentClassId == index;
  }

  initBuffers() {
    for (const cls of this.studentClasses) {
      BufferService.initBuf(cls.data.grpBuffer, this.classLanguage);
    }
  }

  mounted() {
    this.initBuffers();
  }

  updated() {
    this.initBuffers();
  }

  showError(msg: string) {
    this.$bvToast.toast(msg, {
      title: "Error",
      toaster: "b-toaster-top-center",
      variant: "danger",
      solid: true
    });
  }
}
</script>
