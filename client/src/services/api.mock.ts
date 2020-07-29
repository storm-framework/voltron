import {
  UserData,
  User,
  Buffer,
  AuthInfo,
  LoginResponse,
  Roster
} from "@/types";
import router from "@/router";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

const MOCKUSERS: { [userName: string]: User } = {
  rjhala: { firstName: "Ranjit", lastName: "Jhala" },
  nico: { firstName: "Nicolas", lastName: "Lehmann" },
  rose: { firstName: "Rose", lastName: "Kunkel" },
  nadia: { firstName: "Nadia", lastName: "Polikarpova" }
};

const BUFFERS: { [id: string]: Buffer } = {
  0: {
    id: 0,
    hash: "-M9Kx-cxRIUgCqVCtjCr",
    text: "-- Code for group 0\n",
    div: "editor-0"
  },
  1: {
    id: 1,
    hash: "-M9L5YBS0kgvUfuz0Ckc",
    text: "-- Code for group 1\n",
    div: "editor-1"
  },
  2: {
    id: 2,
    hash: "-M9L5oPt0fsruy16vntv",
    text: "-- Code for group 2\n",
    div: "editor-2"
  },
  3: {
    id: 3,
    hash: "-M9L5vCVa5FQ0noobA9V",
    text: "-- Code for group 3\n",
    div: "editor-3"
  },
  4: {
    id: 4,
    hash: "-M9L6XICO2mz_yfpDXWR",
    text: "-- Code for group 4\n",
    div: "editor-4"
  },
  5: {
    id: 5,
    hash: "-M9L6nLdsLy_7aXIs4MX",
    text: "-- Code for group 5\n",
    div: "editor-5"
  }
};

const USERS: { [id: string]: UserData } = {
  rjhala: {
    user: MOCKUSERS["rjhala"],
    classes: [
      {
        tag: "Instructor",
        class: "CSE230",
        allBuffers: [BUFFERS[0], BUFFERS[1], BUFFERS[2]]
      },
      {
        tag: "Instructor",
        class: "CSE130",
        allBuffers: [BUFFERS[3], BUFFERS[4], BUFFERS[5]]
      },
      {
        tag: "Student",
        class: "CSE230",
        grpBuffer: BUFFERS[2]
      }
    ]
  },
  nico: {
    user: MOCKUSERS["nico"],
    classes: [
      {
        tag: "Student",
        class: "CSE130",
        grpBuffer: BUFFERS[4]
      }
    ]
  },
  rose: {
    user: MOCKUSERS["rose"],
    classes: [
      {
        tag: "Student",
        class: "CSE130",
        grpBuffer: BUFFERS[4]
      },
      {
        tag: "Student",
        class: "CSE230",
        grpBuffer: BUFFERS[2]
      }
    ]
  },
  nadia: {
    user: MOCKUSERS["nadia"],
    classes: [
      {
        tag: "Instructor",
        class: "CSE130",
        allBuffers: [BUFFERS[3], BUFFERS[4], BUFFERS[5]]
      }
    ]
  }
};

class ApiService {
  constructor(private accessToken: string | null) {}

  get sessionAccessToken(): string | null {
    return this.accessToken;
  }

  getUserData(name: string): UserData {
    return USERS[name];
  }

  getLoginResponse(name: string): LoginResponse {
    const userData = this.getUserData(name);
    this.accessToken = name;
    return { accessToken: name, user: MOCKUSERS[name] };
  }

  isSignedIn() {
    return this.accessToken !== null;
  }

  async user(token: string): Promise<UserData> {
    await delay();
    return this.getUserData(token);
  }

  async signIn(info: AuthInfo): Promise<LoginResponse> {
    await delay();
    if (
      info.emailAddress == "rjhala@eng.ucsd.edu" &&
      info.password == "rjhala"
    ) {
      return this.getLoginResponse("rjhala");
    }
    if (
      info.emailAddress == "nlehmann@eng.ucsd.edu" &&
      info.password == "nico"
    ) {
      return this.getLoginResponse("nico");
    }
    if (
      info.emailAddress == "wkunkel@eng.ucsd.edu" &&
      info.password == "rose"
    ) {
      return this.getLoginResponse("rose");
    }
    if (info.emailAddress == "nadia@eng.ucsd.edu" && info.password == "nadia") {
      return this.getLoginResponse("nadia");
    }
    return Promise.reject("Bad username and/or password!");
  }

  signOut() {
    this.accessToken = null;
    return Promise.resolve();
  }

  async unauthorized() {
    console.log("unauthorized");
    await this.signOut();
    router.replace({ name: "Login" });
  }

  async enroll(info: Roster) {
    console.log("enroll", info);
    return [];
  }
}

export default new ApiService(null);
