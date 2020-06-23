<template>
<b-container>
  <div class="page-header">
    <b-row>
      <b-col lg="12" md="8" sm="4">
        <h2 class="d-inline">CSE 230</h2>
        <b-button variant="success" size="lg" class="float-right">
          Instructor: {{ instructorName }}
        </b-button>
      </b-col>
    </b-row>
  </div>
    <br />
    <div class="row">
      <div
        class="col-lg-4 col-md-4"
        v-for="dbuf in instructorDivBuffers"
        v-bind:key="dbuf.buf.id"
      >
        <div class="card border-primary mb-4">
          <div class="card-header">Group {{ dbuf.buf.id }}</div>
          <div class="card-body">
            <div v-bind:id="dbuf.div"></div>
          </div>
        </div>
      </div>
    </div>
</b-container>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import BufferService from "@/services/buffer";
import { Buffer, DivBuffer } from "../types";

@Component
export default class Instructor extends Vue {
  name = "Instructor";

  get instructorName() {
    return this.$store.getters.currentUser.name;
  }

  get instructorDivBuffers(): Array<DivBuffer> {
    const bufs: Array<Buffer> = this.$store.getters.instructorBuffers;
    const divBufs = bufs.map(buf => {
      return { buf: buf, div: BufferService.codeBufferDiv(buf) };
    });
    return divBufs;
  }

  mounted() {
    for (const db of this.instructorDivBuffers) {
      BufferService.initBuf(db.buf);
    }
  }
}
</script>
