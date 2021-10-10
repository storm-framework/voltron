import { AuthInfo, Buffer, Roster, User, UserData } from "@/types";

function delay(ms = 1000) {
  if (process.env.NODE_ENV === "development") {
    return new Promise(resolve => setTimeout(resolve, ms));
  } else {
    return Promise.resolve();
  }
}

const MOCKUSERS: { [userName: string]: User } = {
  rjhala: {
    firstName: "Ranjit",
    lastName: "Jhala",
    email: "rjhala@eng.ucsd.edu"
  },
  nico: {
    firstName: "Nicolas",
    lastName: "Lehmann",
    email: "nlehmann@eng.ucsd.edu"
  },
  rose: {
    firstName: "Rose",
    lastName: "Kunkel",
    email: "nlehmann@eng.ucsd.edu"
  },
  nadia: {
    firstName: "Nadia",
    lastName: "Polikarpova",
    email: "nadia@eng.ucsd.edu"
  }
};

const GROUPS = [0, 1, 2, 3, 4, 5];

const BUFFERS: { [id: string]: Buffer } = {
  0: {
    id: 0,
    hash: "-M9Kx-cxRIUgCqVCtjCr",
    text: "-- Code for group 0\n",
    div: "editor-0",
    hide: false
  },
  1: {
    id: 1,
    hash: "-M9L5YBS0kgvUfuz0Ckc",
    text: "-- Code for group 1\n",
    div: "editor-1",
    hide: false
  },
  2: {
    id: 2,
    hash: "-M9L5oPt0fsruy16vntv",
    text: "-- Code for group 2\n",
    div: "editor-2",
    hide: false
  },
  3: {
    id: 3,
    hash: "-M9L5vCVa5FQ0noobA9V",
    text: "-- Code for group 3\n",
    div: "editor-3",
    hide: false
  },
  4: {
    id: 4,
    hash: "-M9L6XICO2mz_yfpDXWR",
    text: "-- Code for group 4\n",
    div: "editor-4",
    hide: false
  },
  5: {
    id: 5,
    hash: "-M9L6nLdsLy_7aXIs4MX",
    text: "-- Code for group 5\n",
    div: "editor-5",
    hide: false
  }
};

const USERS: { [id: string]: UserData } = {
  rjhala: {
    user: MOCKUSERS["rjhala"],
    theme: "",
    keyBinds: "",
    classes: [
      {
        tag: "Instructor",
        class: "CSE230",
        language: "haskell",
        allBuffers: [BUFFERS[0], BUFFERS[1], BUFFERS[2]]
      },
      {
        tag: "Instructor",
        class: "CSE130",
        language: "ocaml",
        allBuffers: [BUFFERS[3], BUFFERS[4], BUFFERS[5]]
      },
      {
        tag: "Student",
        class: "CSE230",
        language: "haskell",
        grpBuffer: BUFFERS[2],
        allGroups: GROUPS
      }
    ]
  },
  nico: {
    user: MOCKUSERS["nico"],
    theme: "",
    keyBinds: "",
    classes: [
      {
        tag: "Student",
        class: "CSE130",
        language: "ocaml",
        grpBuffer: BUFFERS[4],
        allGroups: GROUPS
      }
    ]
  },
  rose: {
    user: MOCKUSERS["rose"],
    theme: "",
    keyBinds: "",
    classes: [
      {
        tag: "Student",
        class: "CSE130",
        language: "ocaml",
        grpBuffer: BUFFERS[4],
        allGroups: GROUPS
      },
      {
        tag: "Student",
        class: "CSE230",
        language: "haskell",
        grpBuffer: BUFFERS[2],
        allGroups: GROUPS
      }
    ]
  },
  nadia: {
    user: MOCKUSERS["nadia"],
    theme: "",
    keyBinds: "",
    classes: [
      {
        tag: "Instructor",
        class: "CSE130",
        language: "ocaml",
        allBuffers: [BUFFERS[3], BUFFERS[4], BUFFERS[5]]
      }
    ]
  }
};

class ApiService {
  getUserData(name: string): UserData {
    return USERS[name];
  }

  async user(token: string): Promise<UserData> {
    await delay();
    return this.getUserData(token);
  }

  async signIn(info: AuthInfo): Promise<User> {
    await delay();
    if (
      info.emailAddress == "rjhala@eng.ucsd.edu" &&
      info.password == "rjhala"
    ) {
      return MOCKUSERS["rjhala"];
    }
    if (
      info.emailAddress == "nlehmann@eng.ucsd.edu" &&
      info.password == "nico"
    ) {
      return MOCKUSERS["nico"];
    }
    if (
      info.emailAddress == "wkunkel@eng.ucsd.edu" &&
      info.password == "rose"
    ) {
      return MOCKUSERS["rose"];
    }
    if (info.emailAddress == "nadia@eng.ucsd.edu" && info.password == "nadia") {
      return MOCKUSERS["nadia"];
    }
    return Promise.reject("Bad username and/or password!");
  }

  signOut() {
    return Promise.resolve();
  }

  async enroll(info: Roster) {
    console.log("enroll", info);
    return [];
  }
}

export default new ApiService();
