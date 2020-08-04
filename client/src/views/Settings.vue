<template>
  <div id="app">
    <div class="container">
      <section class="py-5">
        <div class="row mt-5">
          <div class="col-8 offset-2">
            <h2 class="d-inline">
              Syntax Highlighting
            </h2>

            <br />
            <br />
            <div class="col4">
              <b-dropdown
                id="dropdown-left"
                :text="selectedLang"
                variant="light"
                class="m-2"
              >
                <b-dropdown-item
                  v-for="item in languages"
                  :key="item.index"
                  v-on:click="setLanguage(item)"
                >
                  {{ item }}
                </b-dropdown-item>
              </b-dropdown>
            </div>

            <br />
            <hr />
            <br />

            <h2 class="d-inline">
              New Enrollment
            </h2>
            <b-button
              v-if="!loading"
              variant="success"
              size="lg"
              class="float-right"
              v-on:click="submitEnrolls(newEnrolls)"
            >
              Enroll in {{ currentClass.class }}
            </b-button>
            <br />
            <br />
            <vue-csv-import
              v-model="csv"
              :map-fields="['email', 'firstName', 'lastName', 'group']"
              :table-class="hidetable"
              :auto-match-fields="true"
              :auto-match-ignore-case="true"
              :headers="false"
            >
              <template slot="error">
                File type is invalid
              </template>
              <template slot="next" slot-scope="{ load }">
                <b-button @click.prevent="load">Upload</b-button>
              </template>
            </vue-csv-import>

            <roster-table :enrolls="newEnrolls" emptyText="" />

            <br />
            <hr />
            <br />

            <h2 class="d-inline">
              Current Enrollment
            </h2>
            <br />
            <br />
            <roster-table
              :enrolls="oldEnrolls"
              emptyText="(no students enrolled yet)"
            />
          </div>
        </div>
      </section>
    </div>
  </div>
</template>

<script lang="ts">
import { Vue, Component } from "vue-property-decorator";
import { VueCsvImport } from "vue-csv-import";
import { Roster, EnrollStudent, Buffer, ClassData } from "@/types";
import BufferService from "@/services/buffer";
import RosterTable from "@/components/RosterTable.vue";
import ApiService from "@/services/api";

@Component({
  components: { VueCsvImport, RosterTable }
})
export default class Enroll extends Vue {
  csv: EnrollStudent[] | null = null;
  oldEnrolls: EnrollStudent[] = [];
  fatalError = false;
  fatalErrorMsg = "";
  selectedLang = this.currentClass.language;
  languages = [
    "haskell",
    "java",
    "rust",
    "go",
    "markdown",
    "python",
    "javascript",
    "typescript",
    "prolog",
    "ocaml",
    "latex"
  ];

  get loading() {
    return !this.newEnrolls;
  }

  get currentClass(): ClassData {
    return this.$store.getters.currentClass;
  }

  get newEnrolls() {
    const myCsv = this.csv;
    return myCsv && myCsv.slice(1);
  }

  get hidetable() {
    return "hideme";
  }

  get syntaxCurrentLanguage() {
    return "Syntax Highlighting: " + this.selectedLang;
  }

  private makeEnroll(
    className: string,
    currBufs: Map<number, Buffer>,
    students: EnrollStudent[]
  ): Roster {
    const allGroups = students
      .map(x => x.group)
      .filter(grpId => !currBufs.has(grpId));
    const groups = Array.from(new Set(allGroups));
    const buffers = groups.map(groupId => BufferService.newBuffer(groupId));
    return { class: className, buffers, students };
  }

  setLanguage(language: string) {
    this.selectedLang = language;
    console.log("setLanguage", language);
    const cur = this.currentClass;
    const info = { class: cur.class, language };
    ApiService.setLanguage(info)
      .then(() => this.$store.dispatch("syncSessionUserData"))
      .catch(error =>
        this.showError("Unexpected error: " + error.response?.status)
      );
  }

  submitEnrolls(enrolls: EnrollStudent[]) {
    const cur = this.currentClass;
    console.log("anima-submitEnrolls", enrolls, cur);
    switch (cur.tag) {
      case "Instructor": {
        const bufs = new Map<number, Buffer>(
          cur.allBuffers.map(x => [x.id, x] as [number, Buffer])
        );
        const info = this.makeEnroll(cur.class, bufs, enrolls);

        ApiService.enroll(info)
          .then(newEnrolls => {
            const newStudents = newEnrolls.length - this.oldEnrolls.length;
            const className = this.currentClass.class;
            this.showSuccess(
              `Enrolled ${newStudents} students to ${className}`
            );
            this.csv = null;
            this.oldEnrolls = newEnrolls;
            this.$store.dispatch("syncSessionUserData");
          })
          .catch(error => {
            this.showError("Unexpected error: " + error.response?.status);
          });
      }
    }
  }

  showSuccess(msg: string) {
    this.$bvToast.toast(msg, {
      title: "Success",
      toaster: "b-toaster-top-center",
      variant: "success",
      solid: true
    });
  }

  showError(msg: string) {
    this.$bvToast.toast(msg, {
      title: "Error",
      toaster: "b-toaster-top-center",
      variant: "danger",
      solid: true
    });
  }

  mounted() {
    ApiService.roster(this.currentClass.class)
      .then(enrolls => {
        this.oldEnrolls = enrolls;
      })
      .catch(resp => {
        this.showError("Unexpected error: " + resp);
      });
  }
}
</script>

<style lang="scss">
body {
  .hideme {
    display: none;
  }
}
</style>
