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
package org.apache.myfaces.extensions.validator.core;

import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

/**
 * extensible project stage implementation
 *
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ProjectStage
{
    private ProjectStageName value;

    private ProjectStage(ProjectStageName value)
    {
        this.value = value;
    }

    public static ProjectStageName createStageName(String name)
    {
        return ExtValUtils.createProjectStageName(name);
    }

    public static ProjectStage createStage(ProjectStageName name)
    {
        return new ProjectStage(name);
    }

    public static boolean is(ProjectStageName projectStage)
    {
        return getCurrentProjectStage().equals(projectStage);
    }

    @ToDo(value=Priority.LOW, description="AbstractStartupListener#initProjectStageResolver Sets the resolver in a" +
            "global property. align.")
    private static ProjectStageName getCurrentProjectStage()
    {
        //set ProjectStageResolver to null to tweak the performance
        ProjectStageResolver projectStageResolver = ExtValCoreConfiguration.get().projectStageResolver();

        if(projectStageResolver != null)
        {
            return (projectStageResolver).getCurrentProjectStage().getValue();
        }
        return ExtValUtils.getDefaultStageName();
    }

    public ProjectStageName getValue()
    {
        return this.value;
    }
}
