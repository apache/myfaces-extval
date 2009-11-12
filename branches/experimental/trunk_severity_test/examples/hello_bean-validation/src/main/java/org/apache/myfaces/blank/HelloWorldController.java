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
package org.apache.myfaces.blank;

import org.apache.myfaces.blank.domain.Person;
import org.apache.myfaces.blank.validation.group.Admin;
import org.apache.myfaces.blank.validation.group.User;
import org.apache.myfaces.blank.validation.group.Address;
import org.apache.myfaces.extensions.validator.beanval.annotation.BeanValidation;
import org.apache.myfaces.extensions.validator.beanval.annotation.ModelValidation;

/**
 * A typical simple backing bean, that is backed to <code>helloworld.jsp</code>
 */

//in case of deactivated default validation
@BeanValidation
public class HelloWorldController
{
    //@BeanValidation(viewIds = "/form1.jsp")
    private Person person = new Person();

    /**
     * default empty constructor
     */
    public HelloWorldController()
    {
    }

    /**
     * Method that is backed to a submit button of a form.
     */
    public String send()
    {
        //do real logic, return a string which will be used for the navigation system of JSF
        return "success";
    }

    @BeanValidation.List({
            @BeanValidation(viewIds = "/helloWorld.jsp"),
            @BeanValidation(viewIds = "/form1.jsp", useGroups = User.class),
            @BeanValidation(viewIds = "/form2.jsp", useGroups = Admin.class),
            @BeanValidation(viewIds = "/violation.jsp", useGroups = Admin.class),
            @BeanValidation(viewIds = "/violation.jsp", useGroups = Address.class,
                    modelValidation = @ModelValidation(isActive = true))
    })
    public Person getPerson()
    {
        return person;
    }

    public void setPerson(Person person)
    {
        this.person = person;
    }
}