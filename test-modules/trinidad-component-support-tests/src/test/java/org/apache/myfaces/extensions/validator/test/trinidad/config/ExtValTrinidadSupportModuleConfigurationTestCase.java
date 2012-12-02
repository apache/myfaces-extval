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
package org.apache.myfaces.extensions.validator.test.trinidad.config;

import org.apache.myfaces.extensions.validator.core.ExtValModuleConfiguration;
import org.apache.myfaces.extensions.validator.test.trinidad.AbstractTrinidadSupportTestCase;
import org.apache.myfaces.extensions.validator.trinidad.ExtValTrinidadSupportModuleConfiguration;

/**
 * 
 * since v4
 *
 */
public abstract class ExtValTrinidadSupportModuleConfigurationTestCase extends AbstractTrinidadSupportTestCase
{

    public ExtValTrinidadSupportModuleConfigurationTestCase(String name)
    {
        super(name);
    }

    protected boolean needXmlParameters()
    {
        return getName().contains("Xml");
    }

    protected boolean needCustomConfig()
    {
        return !getName().contains("Xml") && !getName().contains("Default");
    }

    @Override
    /*
     * Made the method final because we do setup by the getCustomConfigObjects
     */
    protected final void invokeStartupListeners()
    {
        super.invokeStartupListeners();
    }
    
    @Override
    protected final ExtValModuleConfiguration[] getCustomConfigObjects()
    {
        if (needCustomConfig())
        {
            ExtValModuleConfiguration[] result = new ExtValModuleConfiguration[]
            { getCustomTrinidadSupportModuleConfiguration() };
            if (result.length == 1 && result[0] == null)
            {
                // test don't want to specify a custom configuration.
                return null;
            } else
            {
                return result;
            }
        } else
        {
            return null;
        }
    }

    abstract protected ExtValTrinidadSupportModuleConfiguration getCustomTrinidadSupportModuleConfiguration();

}
