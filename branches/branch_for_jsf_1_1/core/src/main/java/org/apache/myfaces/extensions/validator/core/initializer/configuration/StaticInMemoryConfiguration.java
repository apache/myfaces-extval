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
import java.util.ArrayList;

/**
 * Implementation of the {@link StaticConfiguration} interface for in-memory configs.
 * It's an implementation which helps to create custom config approaches easily.
 * 
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.REUSE})
public class StaticInMemoryConfiguration implements StaticConfiguration<String, String>
{
    private List<StaticConfigurationEntry<String, String>> mappings
        = new ArrayList<StaticConfigurationEntry<String, String>>();

    public void setSourceOfMapping(String path)
    {
    }

    public List<StaticConfigurationEntry<String, String>> getMapping()
    {
        return mappings;
    }

    public void addMapping(String source, String target)
    {
        StaticConfigurationEntry<String, String> entry = new StaticConfigurationEntry<String, String>();
        entry.setSource(source);
        entry.setTarget(target);
        this.mappings.add(entry);
    }
}
