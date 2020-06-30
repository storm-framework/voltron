<template>
  <b-container>
    <div class="page-header">
      <b-row>
        <b-col lg="12" md="8" sm="4">
          <h2 class="d-inline">{{ className }}</h2>
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
          <div class="card-header">Group {{ studentDivBuffer.buf.id }}</div>
          <div class="card-body">
            <div v-bind:id="studentDivBuffer.div"></div>
          </div>
        </div>
      </b-col>
    </b-row>
  </b-container>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import BufferService from "@/services/buffer";
import { DivBuffer } from "../types";

@Component
export default class Student extends Vue {
  name = "Student";

  get studentName() {
    return this.$store.getters.currentUser.firstName;
  }

  get className() {
    return this.$store.getters.currentClass.class;
  }

  get studentDivBuffer(): DivBuffer {
    const buf = this.$store.getters.studentBuffer;
    const div = BufferService.codeBufferDiv(buf);
    return { buf: buf, div: div };
  }

  mounted() {
    const buf = this.studentDivBuffer.buf;
    BufferService.initBuf(buf);
  }
}
</script>
