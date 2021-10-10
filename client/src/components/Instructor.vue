<template>
  <b-container>
    <div class="page-header" :key="reloadKey">
      <b-row>
        <b-col lg="12" md="8" sm="4">
          <h2 class="d-inline">
            {{ className }} Instructor: {{ instructorName }}
          </h2>
          <!-- <b-button variant="outline-primary" size="lg" class="float-right">
            Instructor: {{ instructorName }}
          </b-button> -->
          <b-button
            variant="success"
            size="lg"
            class="float-right"
            @click="hideBuffers"
          >
            Toggle
          </b-button>
          <b-button variant="success" size="lg" class="float-right">
            Show
          </b-button>
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
        <div class="card border-primary mb-4" v-show="!buf.hide">
          <div class="card-header">
            <!-- <b-button
              :pressed.sync="buf.hide"
              variant="outline-primary"
              @click="hideBuffer(buf)"
            >
              Group {{ buf.id }}
            </b-button> -->
            <b-form-checkbox
              v-model="buf.hide"
              value="true"
              unchecked-value="false"
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
  hiddenBuffers: Set<number> = new Set();
  reloadKey = 0;
  displayMode = 0; // 0 = all, 1 = odd, 2 = even

  get instructorName() {
    return this.$store.getters.currentUser.firstName;
  }

  get className() {
    return this.$store.getters.currentClass.class;
  }

  get instructorBuffers(): Array<Buffer> {
    const bufs: Array<Buffer> = this.$store.getters.instructorBuffers;
    // for (const buf of bufs) {
    //   buf.hide = false;
    // }
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

  // get allVisible(): boolean {
  //   return this.reloadKey == 0;
  // }

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

  isHidden(bufId: number): boolean {
    return this.hiddenBuffers.has(bufId);
  }

  // hideBuffer(buf: Buffer) {
  //   buf.hide = true;
  //   this.hideKey += 1;
  //   this.hiddenBuffers.add(buf.id);
  //   console.log("hideBuffer", buf.id, this.hiddenBuffers.has(buf.id));
  // }

  isShow(buf: Buffer): boolean {
    if (this.displayMode == 0) {
      return true;
    } else {
      return buf.id % 2 == this.displayMode - 1;
    }
  }

  toggle() {
    this.displayMode += 1;
    if (this.displayMode == 3) {
      this.displayMode = 0;
    }
    console.log("toggle", this.displayMode);
  }

  hideBuffers() {
    // for (const buf of this.instructorBuffers) {
    //   console.log("hideBuffer", buf.id, buf.hide);
    // }
    // console.log("FOO - showInstrBufs", this.showInstructorBuffers);
    this.toggle();
    this.reloadKey += 1;
  }

  showBuffers() {
    this.reloadKey = 0;
    this.hiddenBuffers.clear();
  }

  mounted() {
    this.initBuffers(true);
  }

  updated() {
    this.initBuffers(false);
  }
}
</script>
