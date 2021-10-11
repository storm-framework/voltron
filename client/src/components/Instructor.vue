<template>
  <b-container>
    <div class="page-header" :key="reloadKey">
      <b-row>
        <b-col lg="12" md="8" sm="4">
          <h2 class="d-inline">
            {{ className }} Instructor: {{ instructorName }}
          </h2>
          <b-button-group class="float-right" v-if="instructorBuffers.length > 0">
            <b-button variant="outline-secondary" @click="hideBuffers">
              Hide
            </b-button>
            <b-button variant="primary" @click="showBuffers">
              Show
            </b-button>
          </b-button-group>
        </b-col>
      </b-row>
    </div>
    <br />
    <div class="row">
      <div
        class="col-lg-4 col-md-4"
        v-for="buf in showInstructorBuffers"
        v-bind:key="buf.id"
      >
        <div class="card border-primary mb-4">
          <div class="card-header">
            <b-form-checkbox
              v-model="buf.hide"
              value="hide"
              unchecked-value="show"
            >
              Group {{ buf.id }}
            </b-form-checkbox>
          </div>
          <div class="card-body">
            <div v-bind:id="buf.div"></div>
          </div>
        </div>
      </div>
    </div>
    <div v-if="instructorBuffers.length == 0">
      <i>No groups yet, enroll students in the <router-link :to="{ name: 'Settings' }">Settings</router-link> tab.</i>
    </div>
  </b-container>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import BufferService from "@/services/buffer";
import { Buffer } from "../types";

@Component
export default class Instructor extends Vue {
  name = "Instructor";
  initializedBuffers: Set<number> = new Set();
  reloadKey = 0;

  get instructorName() {
    return this.$store.getters.currentUser.firstName;
  }

  get className() {
    return this.$store.getters.currentClass.class;
  }

  get instructorBuffers(): Array<Buffer> {
    const bufs: Array<Buffer> = this.$store.getters.instructorBuffers;
    for (const buf of bufs) {
      const defined = "hide" in buf;
      if (!defined) {
        buf.hide = "show";
      }
    }
    console.log("instructorBuffers", bufs);
    return bufs;
  }

  get showInstructorBuffers(): Array<Buffer> {
    const showBufs = [];
    for (const buf of this.instructorBuffers) {
      if (this.isShow(buf)) {
        showBufs.push(buf);
      } else {
        this.initializedBuffers.delete(buf.id);
      }
    }
    console.log("showInstructorBuffers", showBufs);
    return showBufs;
  }

  initBuffers(all: boolean) {
    for (const buf of this.instructorBuffers) {
      if (all || !this.initializedBuffers.has(buf.id)) {
        if (this.isShow(buf)) {
          BufferService.initBuf(buf, this.$store.getters.currentClass.language);
          this.initializedBuffers.add(buf.id);
        }
      }
    }
  }

  isShow(buf: Buffer): boolean {
    // The second conjunct is forces recomputation of `showInstructorBuffers`
    return buf.hide == "show" && this.reloadKey >= 0;
  }

  hideBuffers() {
    this.reloadKey += 1;
  }

  showBuffers() {
    for (const buf of this.instructorBuffers) {
      buf.hide = "show";
    }
    this.reloadKey = 0;
  }

  mounted() {
    this.initBuffers(true);
  }

  updated() {
    this.initBuffers(false);
  }
}
</script>
