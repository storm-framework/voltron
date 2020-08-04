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
                Group {{ studentClass.data.grpBuffer.id }}
              </div>
              <div class="card-body">
                <div v-bind:id="studentClass.data.grpBuffer.div"></div>
              </div>
            </div>
          </b-col>
        </b-row>
      </div>
    </div>
  </b-container>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import BufferService from "@/services/buffer";
import { Student, ClassView } from "../types";

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
}
</script>
