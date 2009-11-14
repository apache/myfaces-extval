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
package org.apache.myfaces.extensions.validator.test.core.stage;

import org.apache.myfaces.extensions.validator.core.ProjectStage;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import junit.framework.Test;
import junit.framework.TestSuite;

public class ProjectStageTestCase extends AbstractExValCoreTestCase
{
    private static final String PROJECT_STAGE = "javax.faces.PROJECT_STAGE";
    public ProjectStageTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(ProjectStageTestCase.class);
    }

    public void testDevelopmentStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Development");
        assertTrue(ProjectStage.is(ProjectStage.Development));
    }

    public void testUnitTestStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "UnitTest");
        assertTrue(ProjectStage.is(ProjectStage.UnitTest));
    }

    public void testSystemTestStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "SystemTest");
        assertTrue(ProjectStage.is(ProjectStage.SystemTest));
    }

    public void testProductionStage()
    {
        servletContext.addInitParameter(PROJECT_STAGE, "Production");
        assertTrue(ProjectStage.is(ProjectStage.Production));
    }

    public void testDefaultStage()
    {
        assertTrue(ProjectStage.is(ProjectStage.Production));
    }

    public void testWrongDefaultStage1()
    {
        assertFalse(ProjectStage.is(ProjectStage.Development));
    }

    public void testWrongDefaultStage2()
    {
        assertFalse(ProjectStage.is(ProjectStage.UnitTest));
    }

    public void testWrongDefaultStage3()
    {
        assertFalse(ProjectStage.is(ProjectStage.SystemTest));
    }
}