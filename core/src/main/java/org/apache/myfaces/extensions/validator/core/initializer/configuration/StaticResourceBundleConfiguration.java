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
package org.apache.myfaces.extensions.validator.core.initializer.configuration;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.List;
import java.util.ResourceBundle;
import java.util.Enumeration;
import java.util.ArrayList;

/**
 * Default implementation of the {@link StaticConfiguration} interface for property-file based configs.
 *
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.REUSE})
public class StaticResourceBundleConfiguration implements StaticConfiguration<String, String>
{
    private String path;
    private List<StaticConfigurationEntry<String, String>> mappings;

    public void setSourceOfMapping(String path)
    {
        this.path = path;
        //force reload
        mappings = null;
    }

    public List<StaticConfigurationEntry<String, String>> getMapping()
    {
        if(mappings != null)
        {
            return mappings;
        }

        mappings = new ArrayList<StaticConfigurationEntry<String, String>>();

        ResourceBundle mapping = ResourceBundle.getBundle(path);

        if (mapping == null)
        {
            //logging
            return new ArrayList<StaticConfigurationEntry<String, String>>();
        }

        Enumeration keys = mapping.getKeys();

        String metaDataKey;
        String validationStrategyClassName;

        while (keys.hasMoreElements())
        {
            metaDataKey = (String) keys.nextElement();
            validationStrategyClassName = mapping.getString(metaDataKey);

            addMapping(metaDataKey, validationStrategyClassName);
        }
        return mappings;
    }

    private void addMapping(String metaDataKey, String validationStrategyClassName)
    {
        StaticConfigurationEntry<String, String> entry = new StaticConfigurationEntry<String, String>();
        entry.setSource(metaDataKey);
        entry.setTarget(validationStrategyClassName);
        this.mappings.add(entry);
    }
}
