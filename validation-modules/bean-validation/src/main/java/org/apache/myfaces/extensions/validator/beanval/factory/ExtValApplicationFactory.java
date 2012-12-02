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
package org.apache.myfaces.extensions.validator.beanval.factory;

import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.application.ApplicationFactory;
import javax.faces.application.Application;

/**
 * @since 2.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValApplicationFactory extends ApplicationFactory
{
    private ApplicationFactory wrapped;

    public ExtValApplicationFactory(ApplicationFactory wrapped)
    {
        this.wrapped = wrapped;
    }

    public ApplicationFactory getWrapped()
    {
        return this.wrapped.getWrapped();
    }

    @ToDo(value = Priority.HIGH, description = "context param. to deactivate this wrapper")
    public Application getApplication()
    {
        return new ExtValApplicationWrapper(this.wrapped.getApplication());
    }

    public void setApplication(Application application)
    {
        this.wrapped.setApplication(application);
    }
}
