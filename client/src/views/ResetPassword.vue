<template>
  <b-overlay
    :show="loading || sending"
    spinner-variant="primary"
    spinner-type="grow"
    spinner-small
    rounded="sm"
    class="form-signup"
    bg-color="#f5f5f5"
  >
    <b-container class="mt-4">
      <b-alert :show="error" variant="danger" :dismissible="!fatalError">
        {{ errorMsg }}
      </b-alert>
      <b-form @submit.prevent="onSubmit">
        <b-form-group
          label-cols-lg="3"
          label="Reset"
          label-size="lg"
          label-class="font-weight-bold pt-0"
          class="mb-5"
        >
          <b-form-group label="Email address*" label-for="email-address">
            <b-form-input
              id="email-address"
              type="email"
              v-model="emailAddress"
              required
              disabled
            />
          </b-form-group>

          <b-form-group label="Password*" label-for="password">
            <b-form-input
              id="password"
              type="password"
              v-model="password"
              required
              placeholder="Password"
              :disabled="fatalError"
            />
          </b-form-group>

          <b-form-group
            label="Repeat password*"
            label-for="repeat-password"
            :state="validRepeatPassword && null"
            invalid-feedback="Password doesn't match"
          >
            <b-form-input
              id="repeat-password"
              type="password"
              v-model="repeatPassword"
              required
              placeholder="Password"
              :disabled="fatalError"
            />
          </b-form-group>
        </b-form-group>

        <b-form-group label-cols-lg="3">
          <b-button
            :disabled="fatalError || !valid"
            variant="primary"
            size="lg"
            type="submit"
            class="mt-4"
          >
            Reset Password
          </b-button>
        </b-form-group>
      </b-form>
    </b-container>
  </b-overlay>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import ApiService from "@/services/api";
import _ from "lodash";
import { UserSignUp } from "@/models";

interface Form {
  emailAddress: string;
  password: string;
}

@Component({})
export default class SignIn extends Vue {
  emailAddress = "";
  password = "";
  repeatPassword = "";

  validProfile = true;
  loading = false;
  fatalError = false;
  error = false;
  errorMsg = "";
  sending = false;

  get validRepeatPassword() {
    if (_.isEmpty(this.password) || _.isEmpty(this.repeatPassword)) return true;
    return this.password == this.repeatPassword;
  }

  get valid() {
    return this.validRepeatPassword && this.validProfile;
  }

  setError(msg: string, fatal = false) {
    this.errorMsg = msg;
    this.error = true;
    this.fatalError = fatal;
  }

  onSubmit() {
    const code = this.code();
    if (this.sending || !code || !this.valid) {
      return;
    }
    this.sending = true;
    this.submit(code)
      .catch(error => {
        if (error.response?.status == 403) {
          this.setError("Invalid invitation code", true);
        } else if (error.response?.status == 400) {
          this.setError("The form contains errors");
        } else {
          this.setError("There was an unexpected error");
        }
      })
      .finally(() => (this.sending = false));
  }

  async submit(code: string) {
    const data: UserSignUp = {
      invitationCode: code,
      user: {
        emailAddress: this.emailAddress,
        password: this.password,
        ...this.profile,
        photoURL
      }
    };
    await this.$store.dispatch("signUp", data);
    this.$router.replace({ name: "Home" });
  }

  code(): string | null {
    const c = this.$route.query?.code;
    if (typeof c === "string") {
      return c;
    } else {
      return null;
    }
  }
}
</script>

<style lang="scss">
.form-signup {
  width: 100%;
  padding: 15px;
  margin: 0 auto;
}
</style>
