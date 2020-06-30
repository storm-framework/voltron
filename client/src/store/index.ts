import Vue from "vue";
import Vuex from "vuex";
import { LoginResponse, UserData, AuthInfo } from "@/types";
import ApiService from "@/services/api";

Vue.use(Vuex);

type State = {
  userData: UserData | null;
  accessToken: string | null;
  currentClass: number;
};

const initState: State = {
  userData: null,
  accessToken: null,
  currentClass: 0
};

export default new Vuex.Store({
  state: initState,

  mutations: {
    initUser(state, payload: LoginResponse) {
      console.log("mutation-initUser", payload);
      state.userData = payload.user;
      state.accessToken = payload.accessToken;
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

      // axios.post('/verifyPassword?key=[add your Firebase API key here]',{
      //     email: auth.emailAddress,
      //     password: auth.password,
      //     returnSecureToken: truen      })
      //     .then(res => {
      //          console.log(res)
      // })
      //  .catch(error => console.log(error))
    }
  },
  getters: {
    currentClass: ({ userData, currentClass }) => {
      return userData && userData.classes[currentClass];
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
