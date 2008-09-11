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
package org.apache.myfaces.extensions.validator.core.proxy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.internal.UsageEnum;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import javax.faces.application.Application;
import javax.faces.application.ApplicationFactory;

/**
 * @author Gerhard Petracek
 */
@UsageInformation({UsageEnum.ALTERNATIVE, UsageEnum.INTERNAL})
public class ExtValApplicationFactory extends ApplicationFactory
{
    protected final Log logger = LogFactory.getLog(getClass());
    private static boolean active = false;

    private ApplicationFactory wrapped;

    public ExtValApplicationFactory()
    {
    }

    public ExtValApplicationFactory(ApplicationFactory applicationFactory)
    {
        active = true;
        this.wrapped = applicationFactory;
        setApplication(applicationFactory.getApplication());
        logger
            .trace("myfaces-extension-validator application factory instantiated");
    }

    public Application getApplication()
    {
        return this.wrapped.getApplication();
    }

    public void setApplication(Application application)
    {
        if (!(application instanceof ExtValApplication))
        {
            logger.trace("myfaces-extension-validator application created");

            this.wrapped.setApplication(new ExtValApplication(application));
        }
        else
        {
            this.wrapped.setApplication(application);
        }
    }

    public static boolean isActive()
    {
        return active;
    }
}
