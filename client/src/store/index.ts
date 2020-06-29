import Vue from "vue";
import Vuex from "vuex";
import { LoginResponse, UserData, User, AuthInfo } from "@/types";
import ApiService from "@/services/api";

Vue.use(Vuex);

type State = { userData: UserData; sessionUser: User | null };

const initState: State = { userData: { tag: "None" }, sessionUser: null };

export default new Vuex.Store({
  state: initState,

  mutations: {
    setBuffers(state, payload: LoginResponse) {
      console.log("mutation-setBuffers", payload);
      const userData = payload.user;
      switch (userData.tag) {
        case "None":
          return;
        default: {
          state.userData = userData;
          state.sessionUser = userData.info;
        }
      }
    }
  },

  actions: {
    signIn({ commit }, auth: AuthInfo) {
      ApiService.signIn(auth)
        .then(res => {
          console.log("ApiService.signIn", res);
          commit("setBuffers", res);
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
    // userType: ({ userData }) => {
    //   return userData.tag;
    // },

    isInstructor: ({ userData }) => {
      return userData.tag == "Instructor";
    },

    isStudent: ({ userData }) => {
      return userData.tag == "Student";
    },

    currentUser: ({ sessionUser }) => {
      return sessionUser;
    },

    studentBuffer: ({ userData }) => {
      switch (userData.tag) {
        case "Student":
          return userData.grpBuffer;
        default:
          return null;
      }
    },

    instructorBuffers: ({ userData }) => {
      switch (userData.tag) {
        case "Instructor":
          return userData.allBuffers;
        default:
          return null;
      }
    }
  },
  modules: {}
});
