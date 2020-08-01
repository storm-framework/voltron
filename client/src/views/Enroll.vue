<template>
  <div id="app">
    <div class="container">
      <section class="py-5">
        <div class="row mt-5">
          <div class="col-8 offset-2">
            <h2 class="d-inline">
              New Enrollment for {{ currentClass.class }}
            </h2>
            <b-button
              v-if="!loading"
              variant="success"
              size="lg"
              class="float-right"
              v-on:click="submitEnrolls(newEnrolls)"
            >
              Enroll
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

            <br />

            <roster-table :enrolls="newEnrolls" />

            <hr />

            <h2 class="d-inline">
              Current Enrollment for {{ currentClass.class }}
            </h2>
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
