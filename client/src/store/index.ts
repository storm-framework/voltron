import Vue from "vue";
import Vuex from "vuex";
import {
  ClassView,
  LoginResponse,
  UserData,
  AuthInfo,
  Instructor,
  Student,
  Buffer,
  Roster
} from "@/types";
import ApiService from "@/services/api";

Vue.use(Vuex);

type State = {
  userData: UserData | null;
  accessToken: string | null;
  currentClass: number;
};

const initState: State = {
  accessToken: ApiService.sessionAccessToken,
  userData: null,
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
    signOut: ({ commit }) => ApiService.signOut().then(() => commit("signOut")),
    signIn({ dispatch, commit }, auth: AuthInfo) {
      ApiService.signIn(auth)
        .then(res => {
          console.log("ApiService.signIn", res);
          commit("initUser", res);
          dispatch("syncUser", res.accessToken);
        })
        .catch(error => {
          console.log("action-signin-catch", error);
          ApiService.unauthorized();
        });

      return dispatch("syncSessionUserData").then(() =>
        console.log("Done syncSessonUserData")
      );
    },

    syncSessionUserData: ({ dispatch, state }) => {
      console.log("syncSessionUserData", state.accessToken);
      if (state.accessToken !== null) {
        return dispatch("syncUser", state.accessToken);
      } else {
        return Promise.resolve();
      }
    },

    syncUser: ({ commit }, token: string) =>
      ApiService.user(token)
        .then(payload => commit("setUserData", payload))
        .catch(error => {
          if (error?.response?.status == 401) {
            ApiService.unauthorized();
          }
          throw error;
        }),
    
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
            return cur.allBuffers.sort((b1: Buffer, b2: Buffer) => b1.id - b2.id);
          }
        }
      }
      return null;
    }
  },
  modules: {}
});
