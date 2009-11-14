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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * project stage equivalent to jsf 2.0
 * extval 2.x has a special resolver which redirects the call to the new jsf api 
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public enum ProjectStage
{
    Development, UnitTest, SystemTest, Production;

    public static boolean is(ProjectStage projectStage)
    {
        return getCurrentProjectStage().equals(projectStage);
    }

    private static ProjectStage getCurrentProjectStage()
    {
        Object projectStageResolver = ExtValContext.getContext()
                .getGlobalProperty(ProjectStageResolver.class.getName());

        //set it to null to tweek the performance
        if(projectStageResolver instanceof ProjectStageResolver)
        {
            return ((ProjectStageResolver)projectStageResolver).getCurrentProjectStage();
        }
        return ProjectStage.Production;
    }
}
