import ApiService from "@/services/api";
import {
  AuthInfo,
  Buffer,
  ClassView,
  Instructor,
  Student,
  UserData
} from "@/types";
import Vue from "vue";
import Vuex from "vuex";

Vue.use(Vuex);

type State = {
  userData: UserData | null;
  currentClass: number;
};

const initState: State = {
  userData: null,
  currentClass: 0
};

export default new Vuex.Store({
  state: initState,

  mutations: {
    setCurrentClass(state, payload: number) {
      console.log("mutation-updateCurrentClass", payload);
      state.currentClass = payload;
    },
    setUserData(state, payload: UserData) {
      console.log("mutation-setUserData", payload);
      state.userData = payload;
    },
    signOut(state) {
      console.log("sign-out");
      state.userData = null;
      state.currentClass = 0;
    }
  },

  actions: {
    signOut: ({ commit }) => ApiService.signOut().then(() => commit("signOut")),
    signIn: (_, auth: AuthInfo) => ApiService.signIn(auth),
    syncSessionUserData: ({ commit }) =>
      ApiService.userMe().then(payload => commit("setUserData", payload))
  },

  getters: {
    instructorClasses: ({ userData }) => {
      const classes: Array<ClassView<Instructor>> = [];
      userData &&
        userData.classes.forEach((cls, i) => {
          if (cls.tag == "Instructor") {
            classes.push({ name: cls.class, index: i, data: cls });
          }
        });
      return classes;
    },

    studentClasses: ({ userData }) => {
      const classes: Array<ClassView<Student>> = [];
      userData &&
        userData.classes.forEach((cls, i) => {
          if (cls.tag == "Student") {
            classes.push({ name: cls.class, index: i, data: cls });
          }
        });
      return classes;
    },

    currentClassId: ({ currentClass }) => {
      console.log("currentClassId", currentClass);
      return currentClass;
    },

    currentClass: ({ currentClass }, getters) => {
      return getters.classById(currentClass);
      // return userData && userData.classes[currentClass];
    },

    classById: ({ userData }) => (classId: number) => {
      return userData && userData.classes[classId];
    },

    isSignedIn: ({ userData }) => {
      return userData != null;
    },

    isInstructor: (state, getters) => {
      const cur = getters.currentClass;
      return cur && cur.tag == "Instructor";
    },

    isStudent: (state, getters) => {
      const cur = getters.currentClass;
      return cur && cur.tag == "Student";
    },

    currentUser: ({ userData }) => {
      return userData && userData.user;
    },

    studentBuffer: (state, getters) => {
      const cur = getters.currentClass;
      if (cur) {
        switch (cur.tag) {
          case "Student":
            return cur.grpBuffer;
        }
      }
      return null;
    },

    instructorBuffers: (state, getters) => {
      const cur = getters.currentClass;
      if (cur) {
        switch (cur.tag) {
          case "Instructor": {
            return cur.allBuffers.sort(
              (b1: Buffer, b2: Buffer) => b1.id - b2.id
            );
          }
        }
      }
      return null;
    }
  },
  modules: {}
});
