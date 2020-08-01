<template>
  <div v-if="loadedEnrolls" class="mt-2">
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
        <tr v-for="item in enrolls" :key="item.email">
          <td>{{ item.email }}</td>
          <td>{{ item.firstName }}</td>
          <td>{{ item.lastName }}</td>
          <td>{{ item.group }}</td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";
import { EnrollStudent } from "@/types";

@Component({})
export default class RosterTable extends Vue {
  @Prop()
  enrolls?: EnrollStudent[];

  get loadedEnrolls() {
    console.log("loadedEnrolls", this.enrolls);
    return this.enrolls && this.enrolls.length > 0;
  }
}
</script>
