<template>
  <div id="app">
    <div class="container">
      <section class="py-5">
        <div class="row mt-5">
          <div class="col-8 offset-2">
            <h2 class="d-inline">Enrollment for {{ enrollClass.class }}</h2>
            <b-button
              v-if="!loading"
              variant="success"
              size="lg"
              class="float-right"
              v-on:click="submitEnrolls(loadedEnrolls)"
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

              <!-- <template slot="submit" slot-scope="{ submit }">
                <button @submit.prevent="submit">Enroll!</button>
              </template> -->
            </vue-csv-import>

            <br />

            <div v-show="!loading" class="mt-2">
              <table class="table">
                <thead>
                  <tr>
                    <th scope="col">Email</th>
                    <th scope="col">First</th>
                    <th scope="col">Last</th>
                    <th scope="col">Group</th>
                  </tr>
                </thead>
                <tbody>
                  <tr v-for="item in loadedEnrolls" :key="item.email">
                    <td>{{ item.email }}</td>
                    <td>{{ item.firstName }}</td>
                    <td>{{ item.lastName }}</td>
                    <td>{{ item.group }}</td>
                  </tr>
                </tbody>
              </table>
            </div>
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

@Component({
  components: { VueCsvImport }
})
export default class Enroll extends Vue {
  csv: EnrollStudent[] | null = null;

  get loading() {
    return !this.loadedEnrolls;
  }

  get enrollClass(): ClassData {
    const classId = this.$route.params.classId;
    return this.$store.getters.classById(classId);
  }

  get loadedEnrolls() {
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
    const cur = this.enrollClass;
    console.log("anima-submitEnrolls", enrolls, cur);
    switch (cur.tag) {
      case "Instructor": {
        const bufs = new Map<number, Buffer>(
          cur.allBuffers.map(x => [x.id, x] as [number, Buffer])
        );
        const info = this.makeEnroll(cur.class, bufs, enrolls);
        this.$store.dispatch("enroll", info).catch(error => {
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
}
</script>

<style lang="scss">
body {
  .hideme {
    display: none;
  }
}
</style>
