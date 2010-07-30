/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.demo.gui.beans;

import org.apache.myfaces.extensions.validator.demo.domain.Person;
import org.apache.myfaces.extensions.validator.beanval.payload.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.beanval.payload.ViolationSeverity;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.RequestScoped;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@ManagedBean(name = "registrationPage")
@RequestScoped
public class RegistrationPage
{
    @NotNull(payload = DisableClientSideValidation.class)
    private String password;

    @NotNull(payload = ViolationSeverity.Warn.class)
    private String passwordRepeated;

    @ManagedProperty(value = "#{person}")
    private Person person;

    public String finish()
    {
        this.person.setPassword(this.password);
        return "home";
    }

    @Size(min = 6, max = 60)
    public String getEmail()
    {
        return this.person.getEmail();
    }

    public void setEmail(String email)
    {
        this.person.setEmail(email);
    }

    @NotNull
    public int getNumberOfSiblings()
    {
        return this.person.getNumberOfSiblings();
    }

    public void setNumberOfSiblings(int numberOfSiblings)
    {
        this.person.setNumberOfSiblings(numberOfSiblings);
    }

    /*
     * generated
     */
    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

    public String getPasswordRepeated()
    {
        return passwordRepeated;
    }

    public void setPasswordRepeated(String passwordRepeated)
    {
        this.passwordRepeated = passwordRepeated;
    }

    public Person getPerson()
    {
        return person;
    }

    public void setPerson(Person person)
    {
        this.person = person;
    }
}
