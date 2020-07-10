<template>
  <div id="app">
    <div class="container">
      <section class="py-5">
        <div class="row mt-5">
          <div class="col-8 offset-2">
            <h2 class="d-inline">Enrollment for className</h2>
            <b-button
              v-if="!loading"
              variant="success"
              size="lg"
              class="float-right"
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
                </template>-->
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

type EnrollInfo = {
  email: string;
  firstName: string;
  lastName: string;
  group: string;
};

@Component({
  components: { VueCsvImport }
})
export default class Enroll extends Vue {
  csv: Array<EnrollInfo> | null = null;

  get loading() {
    return !this.loadedEnrolls;
  }

  get loadedEnrolls() {
    const myCsv = this.csv;
    return myCsv && myCsv.slice(1);
  }

  get hidetable() {
    return "hideme";
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
