import Vue from "vue";
import Vuex from "vuex";
import { AppState, User, AuthInfo } from "@/types";
import ApiService from "@/services/api";

Vue.use(Vuex);

type State = { buffers: AppState; sessionUser: User | null };

const initState: State = { buffers: { kind: "none" }, sessionUser: null };

export default new Vuex.Store({
  state: initState,

  mutations: {
    setBuffers(state, bufs: AppState) {
      switch (bufs.kind) {
        case "none":
          return;
        default: { 
          console.log("mutation-setBuffers", bufs);
          state.buffers = bufs;
          state.sessionUser = bufs.user;
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
    userType: ({ buffers }) => {
      return buffers.kind; 
    },

    currentUser: ({ sessionUser }) => {
      return sessionUser;
    },

    studentBuffer: ({ buffers }) => {
      switch (buffers.kind) { 
        case "student": return buffers.grpBuffer;
        default: return null;
      }
    },

    instructorBuffers: ({ buffers }) => {
      switch (buffers.kind) { 
        case "instructor": return buffers.allBuffers;
        default: return null;
      }
    },
  },
  modules: {}
});
