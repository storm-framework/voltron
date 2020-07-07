import Vue from "vue";
import Vuex from "vuex";
import { ClassView, LoginResponse, UserData, AuthInfo, Instructor, Student } from "@/types";
import ApiService from "@/services/api";

Vue.use(Vuex);

type State = {
  sessionUserId: string | null;
  userData: UserData | null;
  accessToken: string | null;
  currentClass: number;
};

const initState: State = {
  sessionUserId: ApiService.sessionUserId,
  userData: null,
  accessToken: null,
  currentClass: 0
};

export default new Vuex.Store({
  state: initState,

  mutations: {
    initUser(state, payload: LoginResponse) {
      console.log("mutation-initUser", payload);
      state.accessToken = payload.accessToken;
    },
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
      state.accessToken = null;
      state.currentClass = 0;
    }
  },

  actions: {
    signIn({ commit }, auth: AuthInfo) {
      ApiService.signIn(auth)
        .then(res => {
          console.log("ApiService.signIn", res);
          commit("initUser", res);
          console.log(res);
        })
        .catch(error => console.log("action-signin-catch", error));
    },

    syncSessionUserData: ({ commit, state }) => {
      if (state.sessionUserId !== null) {
        ApiService.user(state.sessionUserId)
          .then(payload => commit("setUserData", payload))
          .catch(error => {
              if (error?.response?.status == 401) {
                ApiService.unauthorized();
              }
              throw error;
            });
      } else {
        ApiService.unauthorized();
      }
    }
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

    currentClass: ({ userData, currentClass }) => {
      return userData && userData.classes[currentClass];
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
          case "Instructor":
            return cur.allBuffers;
        }
      }
      return null;
    }
  },
  modules: {}
});
